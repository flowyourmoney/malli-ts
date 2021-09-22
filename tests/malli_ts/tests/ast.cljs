(ns malli-ts.tests.ast
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [malli-ts.ast :refer [->ast]]
            [malli.core :as m]))

(deftest number
  (is (= (->ast int?) {:type :number}))
  (is (= (->ast :int) {:type :number}))
  (is (= (->ast float?) {:type :number})))


(testing "primitives"
  (number))
