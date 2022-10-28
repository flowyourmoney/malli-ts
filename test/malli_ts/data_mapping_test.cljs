(ns malli-ts.data-mapping-test
  (:require [cljs.test :as t :refer-macros [deftest is testing]]
            [malli-ts.core               :as-alias mts]
            [malli-ts.data-mapping :as sut]
            [malli.core :as m]))

(def *registry
  (let [order-items-schema [:vector
                            [:map
                             [:order-item {:optional      true
                                           ::mts/clj<->js {:prop    "orderItem"
                                                           :fn-to   nil
                                                           :fn-from nil}}
                              [:map
                               [:order-item-id uuid?]
                               [:type {::mts/clj<->js {:prop "type"}}
                                string?]
                               [:price
                                [:map
                                 [:currency [:enum :EUR :USD :ZAR]]
                                 [:amount number?]]]
                               [:test-dummy {::mts/clj<->js {:prop "TESTDummyXYZ"}}
                                string?]
                               [:related-items
                                [:vector [:map
                                          [:how-is-related string?
                                           :order-item-id uuid?]]]]]]
                             [:credit {:optional true}
                              [:map
                               [:valid-for-timespan [:enum :milliseconds :seconds :minutes :hours :days]]
                               [:amount number?]]]]]
        order-schema       [:map
                            [:model-type [:= ::order]]
                            [:order-id {::mts/clj<->js {:prop "orderId"}}
                             string?]
                            [:order-type {::mts/clj<->js {:prop "orderType"}}
                             [:or keyword? string?]]
                            [:order-items {:optional      true
                                           ::mts/clj<->js {:prop "orderItems"}}
                             order-items-schema]
                            [:total-amount {:optional      true
                                            ::mts/clj<->js {:prop "totalAmount"}}
                             number?]
                            [:user {:optional true}
                             [:map
                              [:user-id {::mts/clj<->js {:prop "userId"}} string?]
                              [:name {:optional true} string?]]]]
        s                  [:schema {::mts/t-name  "Order"
                                     ::mts/declare true}
                            order-schema]
        *r                 (atom (m/default-schemas))]
    (swap! *r assoc ::order (m/schema s))
    *r))

(deftest test-a-js-obj-to-clj
  (let [clj-map (sut/to-clj *registry
                            #js {:modelType   ::order
                                 :orderId     "a-test-id-1234"
                                 :orderType   "a-test-wf-type"
                                 :totalAmount 23456.89
                                 :user        #js {:userId "MrTesty"
                                                   :name   "Testy The QA"}
                                 :orderItems  #js [#js {:orderItem
                                                        #js {:type         "some-test-order-item-type-1"
                                                             :price        #js {:currency :EUR
                                                                                :amount   22.3}
                                                             :TESTDummyXYZ "TD-A1"
                                                             :relatedItems #js [#js {:howIsRelated "Dunno"}]}}
                                                   #js {:orderItem
                                                        #js {:type         "some-test-order-item-type-2"
                                                             :price        #js {:currency :ZAR
                                                                                :amount   898}
                                                             :TESTDummyXYZ "TD-B2"}}]})]
    (testing "`to-clj` should map a string"
      (is (= "a-test-id-1234" (:order-id clj-map))))
    (testing "`to-clj` should map a number"
      (is (= 23456.89 (:total-amount clj-map))))
    (testing "`to-clj` should map a value from a nested object"
      (is (= "MrTesty" (get-in clj-map [:user :user-id]))))
    (testing "`to-clj` should map a value from a nested array"
      (is (= :EUR (get-in clj-map [:order-items 0 :order-item :price :currency])))
      (is (= :ZAR (get-in clj-map [:order-items 1 :order-item :price :currency]))))
    (testing "`to-clj` should map a value from a property with a different name"
      (is (= "TD-B2" (get-in clj-map [:order-items 1 :order-item :test-dummy]))))
    (testing "`to-clj` should map a value from a nested array in a nested array"
      (is (= "Dunno" (get-in clj-map [:order-items 0 :order-item :related-items 0 :how-is-related]))))))

(defn- rand-amount [] (* (rand) 100))

(deftest test-js-objs-to-clj
  (let [item-count 2
        js-objs    (-> (map
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
                                                           :relatedItems #js [#js {:howIsRelated (str "Dunno" i)}]}}
                                                 #js {:orderItem
                                                      #js {:type         (str "some-test-order-item-type-B" i)
                                                           :price        #js {:currency :ZAR
                                                                              :amount   (rand-amount)}
                                                           :TESTDummyXYZ (str "TD-B" i) }}]}))
                       (sut/into-js-array (range item-count)))
        clj-maps   (sut/to-clj *registry js-objs)]
    (doall (keep-indexed
            (fn [i clj-map]
              (testing "`to-clj` given an array, should map a string"
                (is (= (str "a-test-id-" i)
                       (:order-id clj-map))))
              (testing "`to-clj` given an array, should map a number"
                (is (number? (:total-amount clj-map))))
              (testing "`to-clj` given an array, should map a value from a nested object"
                (is (= (str "MrTesty" i) (get-in clj-map [:user :user-id]))))
              (testing "`to-clj` given an array, should map a value from a nested array"
                (is (= :EUR (get-in clj-map [:order-items 0 :order-item :price :currency])))
                (is (= :ZAR (get-in clj-map [:order-items 1 :order-item :price :currency]))))
              (testing "`to-clj` given an array, should map a value from a property with a different name"
                (is (= (str "TD-B" i) (get-in clj-map [:order-items 1 :order-item :test-dummy]))))
              (testing "`to-clj` given an array, should map a value from a nested array in a nested array"
                (is (= (str "Dunno" i) (get-in clj-map [:order-items 0 :order-item :related-items 0 :how-is-related])))))
            clj-maps))))

(comment
  (t/run-tests 'malli-ts.data-mapping-test)
  )
