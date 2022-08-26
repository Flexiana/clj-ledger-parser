(ns leedn.integration-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [leedn.parse :as parse]
   [leedn.print :as lpr]
   [matcher-combinators.test]))

(def ledger-file-path "./test/leedn/fixture.dat")

(deftest print-after-parse-test
  (testing "That `print` after `parse` produces the same output."
    (let [ledger-str (-> ledger-file-path slurp str/trim-newline)
          edn        (with-open [r (io/reader ledger-file-path)]
                       (parse/parse-file r))]
      (is (= ledger-str (lpr/render-file edn))))))

(comment

  (let [ledger-str (-> ledger-file-path slurp str/trim-newline)
        edn        (with-open [r (io/reader ledger-file-path)]
                     (parse/parse-file r))]
    (def edn edn)
    (def ledger-str ledger-str)
    (is (= ledger-str (lpr/render-file edn))))
  ;
  )
