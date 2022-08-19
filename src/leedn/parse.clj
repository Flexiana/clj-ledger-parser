(ns leedn.parse
  (:require
   (clj-time
    [coerce :as ctime]
    [core :as time]
    [format :as ftime])
   [clojure.java.io :as io]
   [clojure.string :as str]
   ;; [instaparse.core :as parse]
   [finance.core.types :as types]
   [finance.ledger.parse :as p]
   [instaparse.core :as insta]
   [instaparse.transform :as intr]
   [clojure.test :refer :all]))

(def time-format
  "Formats to accept for time values. This parses dates in the **local**
  time-zone if one is not specified."
  (ftime/formatter
   (time/default-time-zone)
   "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
   "yyyy-MM-dd'T'HH:mm:ss.SSS"
   "yyyy-MM-dd'T'HH:mm:ssZ"
   "yyyy-MM-dd'T'HH:mm:ss"
   "yyyy-MM-dd'T'HH:mmZ"
   "yyyy-MM-dd'T'HH:mm"))

(defn- parse-time
  [date time zone]
  (-> (if zone
        (ftime/with-zone time-format zone)
        time-format)
      (ftime/parse (str date "T" time))))

(defn- date->time
  "Converts a `LocalDate` value into a `DateTime` representing midnight on
  that calendar date in the default time zone."
  [date]
  (-> date
      (ctime/to-date-time)
      (time/from-time-zone (time/default-time-zone))))

(defn- update-time
  "Given an object with date information and potentially some time info, update
  the time attribute with the date information or set it to the start of the
  object's date."
  [obj time-key date-key]
  (if-let [date (get obj date-key)]
    (let [t (get obj time-key)]
      (assoc obj
             time-key
             (if (and (vector? t) (= :Time (first t)))
               (let [[_ time-str zone] t]
                 (parse-time date time-str zone))
               (or t (date->time date)))))
    obj))

(defn- join-field
  [obj field delimiter]
  (let [v (get obj field)]
    (if (sequential? v)
      (assoc obj field (str/join delimiter v))
      obj)))

(defn- assoc-some
  ([x k v]
   (if (some? v)
     (assoc x k v)
     x))
  ([x k v & more]
   (->> more
        (partition-all 2)
        (cons [k v])
        (reduce #(assoc-some %1 (first %2) (second %2)) x))))

(defn- lift-meta
  ([obj meta-tag]
   (lift-meta obj meta-tag meta-tag))
  ([obj meta-tag field-key]
   (lift-meta obj meta-tag field-key identity))
  ([obj meta-tag field-key f]
   (if-let [value (get-in obj [:data/tags meta-tag])]
     (-> obj
         (assoc field-key (f value))
         (update :data/tags dissoc meta-tag)
         (as-> obj'
               (if (empty? (:data/tags obj'))
                 (dissoc obj' :data/tags)
                 obj')))
     obj)))

(defn- distribute-attr
  "Distributes an attribute from a parent entity into a set of child entities,
  keeping any values already present."
  [x attr-key coll-key]
  (if-let [x-attr (get x attr-key)]
    (update
     x coll-key
     (partial mapv
              (fn [entry]
                (if (some? (get entry attr-key))
                  entry
                  (assoc entry attr-key x-attr)))))
    x))

(defn- collect-one
  [k]
  (fn [children]
    (when-let [[match :as matches] (seq (filter #(and (vector? %)
                                                      (= k (first %)))
                                                children))]
      (when (< 1 (count matches))
        (throw (ex-info
                (str "Multiple values present for (collect-one "
                     (pr-str k) ")")
                {:key k, :matches matches})))
      (when (< 2 (count match))
        (throw (ex-info
                (str "Cannot unbox child " (pr-str k) " which has "
                     (dec (count match)) " children")
                {:key k, :match match})))
      (second match))))

(defn- collect-all
  [k]
  (fn [children]
    (when-let [matches (seq (filter #(and (vector? %) (= k (first %)))
                                    children))]
      (when-let [bad-children (seq (filter #(< 2 (count %)) matches))]
        (throw (ex-info
                (str "Cannot unbox " (count bad-children) " " (pr-str k)
                     " children which have too many entries")
                {:key k, :bad-children bad-children})))
      (map second matches))))

(defn- collect-set
  [k]
  (comp not-empty set (collect-all k)))

(defn- collect-map
  [k]
  (fn [children]
    (def k k)
    (def children children)
    (when-let [matches (seq (filter #(and (vector? %)
                                          (= k (first %)))
                                    children))]
      (when-let [bad-children (seq (filter #(not= 2 (count %)) matches))]
        (throw (ex-info
                (str "Cannot unbox " (count bad-children) " " (pr-str k)
                     " children which have the wrong entry counts")
                {:key k, :bad-children bad-children})))
      (into {} (map second matches)))))

(defn- collect
  [initial collectors children]
  (reduce-kv
   (fn [data k collector]
     (if-let [v (collector children)]
       (assoc data k v)
       data))
   initial
   collectors))

(def commodity-transforms
  (merge p/commodity-transforms
         {:CommodityCode
          (fn ->commodity-code
            [code]
            (if (= "$" code) 'USD (symbol code)))

          :CommodityDefinition
          (fn ->commodity
            [code & children]
            (->
             {:commodity/code code}
             (collect
              {:title     (collect-one :NoteDirective)
               :data/tags (collect-map :MetaDirective)
               ::format   (collect-one :CommodityFormat)
               ::options  (collect-all :CommodityOption)}
              children)
             (lift-meta :type :commodity/type (partial keyword "commodity.type"))
             (lift-meta :class :commodity/class (partial keyword "commodity.class"))
             (lift-meta :sector :commodity/sector (partial keyword "commodity.sector"))
             (as-> commodity
                   (let [fmt (::format commodity)]
                     (if (and fmt (not (re-seq #"^\d" fmt)))
                       (assoc commodity :commodity/currency-symbol (subs fmt 0 1))
                       commodity)))
       ; These are unused at the moment.
             (dissoc ::format ::options)))

          :CommodityPrice
          (fn ->commodity-price
            [date code price]
            {:time/at (date->time date)
             :price/commodity code
             :price/value price})}))

(def account-transforms
  {:AccountPathSegment (comp str/join list)
   :AccountPath vector
   :AccountAlias keyword

   :AccountDefinition
   (fn ->account-definition
     [path & children]
     (->
      {:title (last path)
       :account/path path}
      (collect
       {:account/alias (collect-one :AccountAliasDirective)
        ::assertion            (collect-one :AccountAssertion)
        :description           (collect-all :NoteDirective)
        :data/tags             (collect-map :MetaDirective)}
       children)
      (join-field :description "\n")
      (lift-meta :title)
      (lift-meta :type :account/type (partial keyword "account.type"))
      (lift-meta :external-id :account/external-id)
      (lift-meta :link :account/links hash-set)
      (as-> account
            (if-let [commodities (some->>
                                  (::assertion account)
                                  (re-seq #"commodity == \"(\S+)\"")
                                  (map (comp (commodity-transforms :CommodityCode) second))
                                  (set))]
              (assoc account :account/commodities commodities)
              account))
      (dissoc ::assertion)))})

(def metadata-transforms
  {:TagName keyword
   :MetaTag
   (fn ->meta
     ([k]   [k true])
     ([k v] [k v]))

   :SourceMeta
   (fn ->source-meta
     [src line]
     [:SourceMeta [src line]])})

(def transaction-transforms
  {:Transaction
   (fn ->transaction
     [date & children]
     (-> {:data/type :finance/transaction
          :transaction/date date}
         (collect
          {:title                       (collect-one :TxMemo)
           :description                 (collect-all :MetaComment)
           :time/at                     (collect-one :TimeMeta)
           :transaction/flag    (collect-one :TxFlag)
           :transaction/code    (collect-one :TxCode)
           :transaction/entries (collect-all :Posting)
           :data/tags                   (collect-map :MetaEntry)}
          children)
         (join-field :description "\n")
         (update :transaction/entries vec)
         (update-time :time/at :transaction/date)
         (lift-meta :UUID :data/ident (partial str "finance:transaction:"))
         (lift-meta :link :transaction/links hash-set)
         (distribute-attr :time/at :transaction/entries)
         (dissoc :time/at)))

   :TxFlag
   (fn ->flag
     [chr]
     [:TxFlag (case chr "!" :pending, "*" :cleared, :uncleared)])

   :Posting
   (fn ->posting
     [account & children]
     (let [posting-type (case (first account)
                          :RealAccountRef :real
                          :VirtualAccountRef :virtual
                          :BalancedVirtualAccountRef :balanced-virtual)
           [amount children] (if (vector? (first children))
                               [nil children]
                               [(first children) (rest children)])]
       [:Posting
        (->
         {:data/type :entry/posting
          :entry/account (second account)}
         (assoc-some
          :posting/amount amount)
         (collect
          {:entry/source-lines (collect-set :SourceMeta)
           :balance/amount     (collect-one :PostingBalance)
           :posting/price      (collect-one :PostingPrice)
           :posting/invoice    (collect-all :LineItem)
           ::lot-cost                  (collect-one :PostingLotCost)
           ::lot-date                  (collect-one :PostingLotDate)
           ::date                      (collect-one :PostingDate)
           :time/at                    (collect-one :TimeMeta)
           :data/tags                  (collect-map :MetaEntry)
           :description                (collect-all :MetaComment)}
          children)
          ; TODO: (lift-meta :interval :time/interval ...)
          ; Default :time/at to the start of :time/interval if missing
         (update-time :time/at ::date)
         (dissoc ::date)
         (join-field :description "\n")
         (lift-meta :type :data/type (partial keyword "entry"))
         (lift-meta :Payee :posting/payee)
         (lift-meta :external-id :entry/external-id)
         (as-> posting
               (cond-> posting
                 (::lot-cost posting)
                 (update :posting/cost assoc :amount (::lot-cost posting))
                 (::lot-date posting)
                 (update :posting/cost assoc :date (::lot-date posting))
                 (seq (:posting/invoice posting))
                 (assoc :posting/invoice
                        {:data/type :finance/invoice
                         :invoice/items (vec (:posting/invoice posting))})
                 (= posting-type :virtual)
                 (assoc :posting/virtual true)
              ; Automatically detect balance-check entries.
                 (and (or (nil? (:value amount))
                          (zero? (:value amount)))
                      (= :balanced-virtual posting-type)
                      (:balance/amount posting))
                 (-> (assoc :data/type :entry/balance-check)
                     (dissoc :posting/amount))
              ; If type is overridden and amount is zero, remove it.
                 (and (not= :entry/posting (:data/type posting))
                      (or (nil? (:value amount)) (zero? (:value amount))))
                 (dissoc :posting/amount)))
         (dissoc ::lot-cost ::lot-date))]))

   :LineItemTaxGroup keyword
   :LineItemTaxGroups
   (fn ->tax-groups
     [& groups]
     [:LineItemTaxGroups (set groups)])

   :LineItem
   (fn ->line-item
     [desc & children]
     [:LineItem
      (collect
       {:title desc
        :data/type :finance/item}
       {:item/total       (collect-one :LineItemTotal)
        :item/amount      (collect-one :LineItemAmount)
        :item/price       (collect-one :LineItemPrice)
        :item/tax-groups  (collect-one :LineItemTaxGroups)
        :item/tax-applied (collect-one :LineItemTaxApplied)}
       children)])})

(def ledger-transforms
  (merge
   p/ledger-transforms
   p/basic-transforms
   commodity-transforms
   account-transforms
   transaction-transforms))

(defn interpret-parse
  [tree]
  (def tree tree)
  (try
    (intr/transform ledger-transforms tree)
    (catch Exception e
      (throw (ex-info (str "Failed to interpret parse tree: " tree)
                      {:tree tree}
                      e)))))

(defn parse-group [text]
  (->> text
       (p/ledger-parser)
       ;; (check-parse!)
       (interpret-parse)
       ;; (map (partial add-source text))
       ))

(defn parse-file
  "Parse a single file, returning a sequence of interpreted ledger entries."
  [file]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (p/group-lines)
       (mapcat parse-group)))

(comment
  (-> "resources/journal.dat"
      parse-file)

;-)
  )


