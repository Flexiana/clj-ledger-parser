(ns instaparse-repl
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [finance.ledger.parse :as p]
   [instaparse.core :as insta]))

(def current-parser (insta/parser (io/resource "grammar/ledger.bnf")))
(def proposed-parser (insta/parser (io/resource "grammar/ledger.y")))

(def ledger-file-path "./../test/leedn/fixture.dat")

(comment
  (-> ledger-file-path
      io/resource
      slurp
      proposed-parser
      pprint)

  (with-open [r (io/reader (io/resource ledger-file-path))]
    (->> r
         (line-seq)
         (p/group-lines)
         (mapcat current-parser)))

  ;-
  )
