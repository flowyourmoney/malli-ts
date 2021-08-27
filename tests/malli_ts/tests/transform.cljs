(ns malli-ts.tests.transform
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [malli-ts.ast :refer [transform]]
            [malli.core :as m]))

(deftest number
  (is (transform int?) :number)
  (is (transform :int) :number)
  (is (transform float?) :number))


(testing "primitives"
  (number))
