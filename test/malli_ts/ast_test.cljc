(ns malli-ts.ast-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [malli-ts.ast :refer [->ast]]))

(deftest number
  (is (= :number (get (->ast int?) :type)))
  (is (= :number (get (->ast int?) :type)))
  (is (= :number (get (->ast int?) :type))))


(comment
  (testing "primitives"
    (number)))
