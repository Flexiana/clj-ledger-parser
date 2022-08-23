(ns instaparse-repl
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [instaparse.core :as insta]))

(def parser (insta/parser (io/resource "grammar/ledger.y")))

(comment
  (-> "journal.dat"
      io/resource
      slurp
      parser
      pprint))
