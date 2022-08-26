(ns leedn.print
  "Ledger file printing code."
  (:require
   (clj-time
    [coerce :as ctime]
    [core :as time]
    [format :as ftime])
   [clojure.java.io :as io]
   [clojure.string :as str]
   [leedn.parse :as p]))

;; ## Rendering Functions

(defmulti render-entry
  "Renders the given ledger entry as a string."
  :data/type)

(defn render-file
  "Renders a ledger file from a sequence of entries. Returns a string of the
  file contents."
  [entries]
  (def entries entries)
  (str/join "\n\n" (map (comp str/trim render-entry) entries)))

;; ## Entry Rendering Methods

(defn render-quantity
  "Renders a financial quantity."
  [{:keys [commodity value]}]
  ; TODO: this should allow for general format options
  ; TODO: support commas
  (case commodity
    USD (format "$%.2f" value)
    (format "%.6f %s" value commodity)))

(defn render-posting
  [posting]
  (def posting posting)
  (let [{account :entry/account
         amount :posting/amount
         time :time/at} posting
        account-name (if (vector? account)
                       (str/join ":" account)
                       (name account))
        _unused (dissoc posting :account)]
    (str
     (if amount
       (format "    %s  %s" account-name (render-quantity amount))
       (format "    %s" account-name))
      ; TODO: lot cost, lot date, price, balance
     #_(when time
         (str "\n        ; time: " time))
      ; TODO: items
     (when-let [sources (seq (:sources posting))]
       (str/join (map #(str "\n        ; source: " (name (:source %)) "|" (:line %)) sources)))
     #_(when-let [unused (not-empty (dissoc posting :account :amount :time :sources))]
         (str "\n        ; unused: " (pr-str unused))))))

(defn render-transaction
  [tx]
  (def tx tx)
  (let [{:tx/keys [date flag entries code time]} tx]
    (str #_(when-let [unused (not-empty (dissoc tx :parse/source :date :title :status :time :postings))]
             (str "; " (pr-str unused) "\n"))
     date
         (case flag :cleared " *" :pending " !" nil)
         (when code
           (str " (" code ")"))
         " " (:title tx) "\n"
         (when time
           (str "    ; time: " time "\n"))
                                        ; meta
                                        ; comments
         (when-let [postings (seq entries)]
                                        ; TODO: when there are only two postings and no special costs or prices, elide the negative posting amount?
           (str/join "\n" (map render-posting postings))))))

(defmethod render-entry :ledger/comment
  [comment]
  (str "; " (:comment/text comment))) ;; TODO: support different comment styles

(defmethod render-entry :finance/account
  [{:account/keys [alias assertion note path]}]
  (str "account " (str/join ":" path) "\n"
       (when alias
         (str "    alias " (name alias) "\n"))
       (when assertion
         (str "    assert " assertion "\n"))
       (when note
         (str "    note " note "\n"))))

(defmethod render-entry :finance/transaction
  [tx]
  (def tx tx)
  (render-transaction tx))

(defmethod render-entry :default
  [entry]
  (def entry entry)
  (str "; Unknown entry " (pr-str entry)))

(comment
  (do
      (require '[leedn.parse :as p])

      (->> (p/parse-file (io/reader "./resources/journal.dat"))
           (def rr))

      (->> rr
           ;; (filter #(not= :finance/account (:data/type %)))
           ;; (take 1)
           (render-file) ;; TODO: find bug
         ;; (spit "./spited.dat" )
           )
      ;
      )

  (spit "/tmp/spited.dat" (render-file rr))

  (io/delete-file "./spited.dat")

  (render-entry (first entries))
  
  )

