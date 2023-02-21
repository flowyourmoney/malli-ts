(ns malli-ts.data-mapping-test
  (:require [cljs.test :as t :refer-macros [deftest is testing]]
            [malli-ts.core               :as-alias mts]
            [malli-ts.data-mapping :as sut]
            [malli-ts.data-mapping.to-clj :as sut-tc]
            [malli-ts.data-mapping.to-js :as sut-tj]
            [malli.core :as m]))

(def schema
  (let [order-items-schema [:vector [:map
                                     [:order/item {:optional      true
                                                   ::mts/clj<->js {:prop    "orderItem"
                                                                   :fn-to   nil
                                                                   :fn-from nil}}
                                      [:map
                                       [:order-item/id uuid?]
                                       [:order-item/type {::mts/clj<->js {:prop "type"}}
                                        string?]
                                       [:order-item/price
                                        [:map
                                         [:order-item/currency [:enum :EUR :USD :ZAR]]
                                         [:order-item/amount number?]]]
                                       [:order-item/test-dummy {::mts/clj<->js {:prop "TESTDummyXYZ"}}
                                        string?]
                                       [:order-item/related-items
                                        [:ref ::order-items]]]]
                                     [:order/credit {:optional true}
                                      [:map
                                       [:order.credit/valid-for-timespan [:enum :milliseconds :seconds :minutes :hours :days]]
                                       [:order.credit/amount number?]]]]]
        order-schema       [:map
                            [:model-type [:= ::order]]
                            [:order/id {::mts/clj<->js {:prop "orderId"}}
                             string?]
                            [:order/type {::mts/clj<->js {:prop "orderType"}}
                             [:or keyword? string?]]
                            [:order/items {:optional      true
                                           ::mts/clj<->js {:prop "orderItems"}} ::order-items]
                            [:order/total-amount {:optional      true
                                                  ::mts/clj<->js {:prop "totalAmount"}}
                             number?]
                            [:order/user {::mts/clj<->js {:prop "user"}
                                          :optional      true}
                             [:map
                              [:user/id {::mts/clj<->js {:prop "userId"}} string?]
                              [:user/name {:optional true} string?]]]]]
    (m/schema [:schema {:registry {::order-items order-items-schema}}
               order-schema])))

(def mapping (sut/clj<->js-mapping schema {:default-to-camel-case true}))

(deftest root-reference
  (let [root (m/schema [:schema {:registry {::root schema}} ::root])
        clj-map
        {:model-type   ::order
         :order/id     "a-root-id-1234"
         :order/type   "Reference Gear"} 
        js-obj (sut-tj/to-js clj-map {} root)]
    (testing "to-js with a one-off mapping to root reference should work"
      (is (m/validate root clj-map))
      (is (= "Reference Gear" (:order/type clj-map)))
      (is (= "Reference Gear" (aget js-obj "orderType"))))))

;; TODO:
;; 1. Add another test for references
;; 2. For duplicate property names in different locations in the schema

(deftest test-a-js-obj-to-clj
  (let [clj-map (sut-tc/to-clj
                 #js {:modelType   ::order
                      :orderId     "a-test-id-1234"
                      :orderType   "Sport Gear"
                      :totalAmount 23456.89
                      :user        #js {:userId "MrTesty"
                                        :name   "Testy The QA"}
                      :orderItems  #js [#js {:orderItem
                                             #js {:type         "some-test-order-item-type-1"
                                                  :price        #js {:currency :EUR
                                                                     :amount   22.3}
                                                  :TESTDummyXYZ "TD-A1"
                                                  :relatedItems #js [#js {:credit
                                                                          #js {:amount 676.30}}]}}
                                        #js {:orderItem
                                             #js {:type         "some-test-order-item-type-2"
                                                  :price        #js {:currency :ZAR
                                                                     :amount   898}
                                                  :TESTDummyXYZ "TD-B2"}}]}
                 mapping)]
    (do
      #_(testing  "`pr-str` should be clojure readable"
        (is (= "{:model-type :malli-ts.data-mapping-test/order, :order/id \"a-test-id-1234\", :order/type \"Sport Gear\", :order/total-amount 23456.89, :order/user {:user/id \"MrTesty\", :user/name \"Testy The QA\"}, :order/items [{:order/item {:order-item/type \"some-test-order-item-type-1\", :order-item/price {:order-item/currency :EUR, :order-item/amount 22.3}, :order-item/test-dummy \"TD-A1\", :order-item/related-items [{:order/credit {:order.credit/amount 676.3}}]}} {:order/item {:order-item/type \"some-test-order-item-type-2\", :order-item/price {:order-item/currency :ZAR, :order-item/amount 898}, :order-item/test-dummy \"TD-B2\"}}]}" (pr-str clj-map))))
      (testing "`to-clj` should map a string"
        (is (= "a-test-id-1234" (:order/id clj-map))))
      (testing "`to-clj` should map a number"
        (is (= 23456.89 (:order/total-amount clj-map))))
      (testing "`to-clj` should map a value from a nested object"
        (is (= "MrTesty" (get-in clj-map [:order/user :user/id]))))
      (testing "`to-clj` should map a value from a nested array"
        (is (= :EUR (get-in clj-map [:order/items 0 :order/item :order-item/price :order-item/currency])))
        (is (= :ZAR (get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency]))))
      (testing "`to-clj` should map a value from a property with a different name"
        (is (= "TD-B2" (get-in clj-map [:order/items 1 :order/item :order-item/test-dummy]))))
      (testing "`to-clj` should map a value from a nested array in a nested array"
        (is (= 676.30 (get-in clj-map [:order/items 0 :order/item :order-item/related-items
                                       0 :order/credit :order.credit/amount])))))))

(deftest a-regular-clj-object
  (let [clj-map
                  {:model-type   ::order
                      :order/id     "a-test-id-1234"
                      :order/type   "Sport Gear"
                      :order/total-amount 23456.89
                      :order/user         {:user/id "MrTesty"
                                        :user/name   "Testy The QA"}
                      :order/items   [ {:order/item
                                              {:order-item/type         "some-test-order-item-type-1"
                                                  :order-item/price         {:order-item/currency :EUR
                                                                     :order-item/amount   22.3}
                                                  :order-item/test-dummy "TD-A1"
                                                  :order-item/related-items  [ {:order/credit
                                                                           {:order.credit/amount 676.30}}]}}
                                         {:order/item
                                              {:order-item/type         "some-test-order-item-type-2"
                                                  :order-item/price         {:order-item/currency :ZAR
                                                                     :order-item/amount   898}
                                                  :order-item/test-dummy "TD-B2"}}]}]
    (do
      #_(testing  "`pr-str` should be clojure readable"
        (is (= "{:model-type :malli-ts.data-mapping-test/order, :order/id \"a-test-id-1234\", :order/type \"Sport Gear\", :order/total-amount 23456.89, :order/user {:user/id \"MrTesty\", :user/name \"Testy The QA\"}, :order/items [{:order/item {:order-item/type \"some-test-order-item-type-1\", :order-item/price {:order-item/currency :EUR, :order-item/amount 22.3}, :order-item/test-dummy \"TD-A1\", :order-item/related-items [{:order/credit {:order.credit/amount 676.3}}]}} {:order/item {:order-item/type \"some-test-order-item-type-2\", :order-item/price {:order-item/currency :ZAR, :order-item/amount 898}, :order-item/test-dummy \"TD-B2\"}}]}" (pr-str clj-map))))
      (testing "`to-clj` should map a string"
        (is (= "a-test-id-1234" (:order/id clj-map))))
      (testing "`to-clj` should map a number"
        (is (= 23456.89 (:order/total-amount clj-map))))
      (testing "`to-clj` should map a value from a nested object"
        (is (= "MrTesty" (get-in clj-map [:order/user :user/id]))))
      (testing "`to-clj` should map a value from a nested array"
        (is (= :EUR (get-in clj-map [:order/items 0 :order/item :order-item/price :order-item/currency])))
        (is (= :ZAR (get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency]))))
      (testing "`to-clj` should map a value from a property with a different name"
        (is (= "TD-B2" (get-in clj-map [:order/items 1 :order/item :order-item/test-dummy]))))
      (testing "`to-clj` should map a value from a nested array in a nested array"
        (is (= 676.30 (get-in clj-map [:order/items 0 :order/item :order-item/related-items
                                       0 :order/credit :order.credit/amount])))))))

(defn- rand-amount [] (* (rand) 100))

(deftest test-js-objs-to-clj
  (let [item-count 20
        js-objs
        (->>
         (range item-count)
         (map
          (fn [i]
            #js {:modelType   ::order
                 :orderId     (str "a-test-id-" i)
                 :orderType   (str "a-test-wf-type" i)
                 :totalAmount (rand-amount)
                 :user        #js {:userId (str "MrTesty" i)
                                   :name   (str "Testy The QA" i)}
                 :orderItems  #js [#js {:orderItem
                                        #js {:type         (str "some-test-order-item-type-A" i)
                                             :price        #js {:currency :EUR
                                                                :amount   (rand-amount)}
                                             :TESTDummyXYZ (str "TD-A" i)
                                             :relatedItems #js [#js {:credit
                                                                     #js {:amount (inc (rand-amount))}}]}}
                                   #js {:orderItem
                                        #js {:type         (str "some-test-order-item-type-B" i)
                                             :price        #js {:currency :ZAR
                                                                :amount   (rand-amount)}
                                             :TESTDummyXYZ (str "TD-B" i)}}]}))
         (apply array))
        clj-maps   (sut-tc/to-clj js-objs mapping)]
    (doall
     (keep-indexed
      (fn [i clj-map]
        (testing  "`to-clj` given an array, should map a string"
          (is (= (str "a-test-id-" i) (:order/id clj-map))))
        (testing "`to-clj` given an array, should map a number"
          (is (number? (:order/total-amount clj-map))))
        (testing  "`to-clj` given an array, should map a value from a nested object"
          (is (= (str "MrTesty" i) (get-in clj-map [:order/user :user/id]))))
        (testing  "`to-clj` given an array, should map a value from a nested array"
          (is (= :EUR (get-in clj-map [:order/items 0 :order/item :order-item/price :order-item/currency])))
          (is (= :ZAR (get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency]))))
        (testing  "`to-clj` given an array, should map a value from a property with a different name"
          (is (= (str "TD-B" i) (get-in clj-map [:order/items 1 :order/item :order-item/test-dummy]))))
        (testing "`to-clj` given an array, should map a value from a nested array in a nested array"
          (is (< 0 (get-in clj-map [:order/items 0 :order/item :order-item/related-items
                                    0 :order/credit :order.credit/amount])))))
      clj-maps))))

(deftest test-a-clj-map-to-js
  (let [order-id      "a-test-id-1234"
        total-amount  23456.89
        user-id       "MrTesty"
        currency1     :EUR
        currency2     :ZAR
        test-dummy    "TD-B2"
        credit-amount 676.30
        js-obj        (sut-tj/to-js {:model-type         ::order
                                     :order/id           order-id
                                     :order/type         "Sport Gear"
                                     :order/total-amount total-amount
                                     :order/user         {:user/id   user-id
                                                          :user/name "Testy The QA"}
                                     :order/items        [{:order/item
                                                           {:order-item/type          "some-test-order-item-type-1"
                                                            :order-item/price         {:order-item/currency currency1
                                                                                       :order-item/amount   22.3}
                                                            :order-item/test-dummy    "TD-A1"
                                                            :order-item/related-items [{:order/credit
                                                                                        {:order.credit/amount credit-amount}}]}}
                                                          {:order/item
                                                           {:order-item/type       "some-test-order-item-type-2"
                                                            :order-item/price      {:order-item/currency currency2
                                                                                    :order-item/amount   898}
                                                            :order-item/test-dummy test-dummy}}]}
                                    mapping)]
    (testing "`to-js` should map a string"
      (is (= order-id (aget js-obj "orderId"))))
    (testing "`to-js` should map a number"
      (is (= total-amount (aget js-obj "totalAmount"))))
    (testing "`to-js` should map a value from a nested map"
      (is (= user-id (aget js-obj "user" "userId"))))
    (testing "`to-js` should map a value from a nested vector"
      (is (= currency1 (aget js-obj "orderItems" 0 "orderItem" "price" "currency")))
      (is (= currency2 (aget js-obj "orderItems" 1 "orderItem" "price" "currency"))))
    (testing "`to-js` should map a value to a property with a different name"
      (is (= test-dummy (aget js-obj "orderItems" 1 "orderItem"  "TESTDummyXYZ"))))
    (testing "`to-js` should map a value from a nested vector in a nested vector"
      (is (= credit-amount
             (aget js-obj "orderItems" 0 "orderItem" "relatedItems" 0 "credit" "amount"))))))

(deftest test-clj-maps-to-js
  (let [item-count 20
        order-id   "a-test-id-"
        order-type "a-test-order-type-"
        user-id    "MrTesty"
        user-name  "Testy The QA"
        currency1  :EUR
        currency2  :ZAR
        test-dummy "TD-B"
        clj-maps   (->> item-count
                        range
                        (mapv
                         (fn [i]
                           {:model-type         ::order
                            :order/id           (str order-id i)
                            :order/type         (str order-type i)
                            :order/total-amount (rand-amount)
                            :order/user         {:user/id   (str user-id i)
                                                 :user/name (str user-name i)}
                            :order/items        [{:order/item
                                                  {:order-item/type          "some-test-order-item-type-1"
                                                   :order-item/price         {:order-item/currency currency1
                                                                              :order-item/amount   (rand-amount)}
                                                   :order-item/test-dummy    "TD-A1"
                                                   :order-item/related-items [{:order/credit
                                                                               {:order.credit/amount (rand-amount)}}]}}
                                                 {:order/item
                                                  {:order-item/type       "some-test-order-item-type-2"
                                                   :order-item/price      {:order-item/currency currency2
                                                                           :order-item/amount   (rand-amount)}
                                                   :order-item/test-dummy (str test-dummy i)}}]})))
        js-objs    (sut-tj/to-js clj-maps mapping)]
    (doall (keep-indexed
            (fn [i js-obj]
              (testing "`to-js` given a vector, should map a string"
                (is (=  (str order-id i) (aget js-obj 'orderId))))
              (testing "`to-js` given a vector, should map a number"
                (is (number? (aget js-obj "totalAmount"))))
              (testing  "`to-js` given a vector, should map a value from a nested map"
                (is (= (str user-id i) (aget js-obj "user" "userId"))))
              (testing  "`to-js` given a vector, should map a value from a nested verctor"
                (is (= currency1 (aget js-obj "orderItems" 0 "orderItem" "price" "currency")))
                (is (= currency2 (aget js-obj "orderItems" 1 "orderItem" "price" "currency"))))
              (testing  "`to-js` given a vector, should map a value to a property with a different name"
                (is (= (str test-dummy i) (aget js-obj "orderItems" 1 "orderItem"  "TESTDummyXYZ"))))
              (testing "`to-js` should map a value from a nested vector in a nested vector"
                (is (number? (aget js-obj "orderItems" 0
                                   "orderItem" "relatedItems" 0 "credit" "amount")))))
            js-objs))))

(doseq [x (range 7)]
   ;; Benchmark run above tests
  (simple-benchmark [] (a-regular-clj-object) 10000)
  (simple-benchmark [] (test-a-js-obj-to-clj) 10000)
  (simple-benchmark [] (test-a-clj-map-to-js) 10000)
  (println))

(comment
  (t/run-tests 'malli-ts.data-mapping-test)

  (t/test-vars [#'malli-ts.data-mapping-test/test-a-clj-map-to-js])
  (t/test-vars [#'malli-ts.data-mapping-test/test-js-objs-to-clj])
  
)
