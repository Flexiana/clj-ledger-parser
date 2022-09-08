(ns instaparse-repl
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [finance.ledger.parse :as p]
   [instaparse.core :as insta]))

(def current-parser (insta/parser (io/resource "grammar/ledger.bnf")))
(def proposed-parser (insta/parser (io/resource "grammar/ledger.y")))

(def ledger-file-path (io/resource "small-journal.dat"))

(comment
  (-> ledger-file-path
      slurp
      proposed-parser
      pprint)

  (-> ledger-file-path
      slurp
      current-parser
      pprint)

  (with-open [r (io/reader (io/resource ledger-file-path))]
    (->> r
         (line-seq)
         (p/group-lines)
         (mapcat current-parser)))

  ;-
  )
