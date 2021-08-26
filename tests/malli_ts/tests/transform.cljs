(ns malli-ts.tests.transform
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [malli-ts.transform :refer [transform]]
            [malli.core :as m]))

(deftest number
  (is (transform int?) :number)
  (is (transform :int) :number)
  (is (transform float?) :number))


(testing "primitives"
  (number))
