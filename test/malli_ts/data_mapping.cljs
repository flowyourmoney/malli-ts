(ns malli-ts.tests.data-mapping
  (:require [cljs.test :as t :refer-macros [deftest is testing]]
            [malli-ts.core               :as-alias mts]
            [malli-ts.data-mapping :as sut]
            [malli.core :as m]))

(deftest test-to-clj
  (let [order-items-schema [:vector
                            [:map
                             [:order-item {:optional      true
                                           ::mts/clj<->js {:prop    "orderItem"
                                                           :fn-to   nil
                                                           :fn-from nil}}
                              [:map
                               [:type {::mts/clj<->js {:prop "type"}}
                                string?]
                               [:args any?]
                               [:test-dummy {::mts/clj<->js {:prop "testDummy"}}
                                string?]]]
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
        *registry          (atom (m/default-schemas))
        _                  (swap! *registry assoc ::order (m/schema s))
        clj-map            (sut/to-clj *registry
                                       #js {:modelType   ::order
                                            :orderId     "a-test-id-1234"
                                            :orderType   "a-test-wf-type"
                                            :totalAmount 23456.89
                                            :user        {:userId "MrTesty"
                                                          :name   "Testy The QA"}
                                            :orderItems  #js [#js
                                                              {:activity #js
                                                               {:type      "some-test-activity-type-1"
                                                                :args      {:arg1 "dddd"
                                                                            :arg2 22.3}
                                                                :testDummy "dhjhjhdjhjd"}}]})
        ]
    (testing "`to-clj` should map a string"
      (is (= "a-test-id-1234" (:order-id clj-map))))
    (testing "`to-clj` should map a number"
      (is (= 23456.89 (:total-amount clj-map))))
    (testing "`to-clj` should map a value from a nested map"
      (is (= "MrTesty" (get-in clj-map [:user :user-id]))))))

(comment
  (t/run-tests 'malli-ts.tests.data-mapping)
  )
