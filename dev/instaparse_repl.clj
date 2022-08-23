(ns instaparse-repl
  (:require
   (clj-time
     [coerce :as ctime]
     [core :as time]
     [format :as ftime])
   [clojure.java.io :as io]
   [clojure.string :as str]
   [finance.core.types :as types]
   [finance.ledger.parse :as p]
   [instaparse.core :as insta]
   [instaparse.transform :as intr]
   [clojure.spec :as s]
   [clojure.test :refer :all]
   )
  )

(comment


  (insta/parser (io/resource "grammar/ledger.y"))


  )
