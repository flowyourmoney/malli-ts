(ns malli-ts.data-mapping-test
  (:require [cljs.test :as t :refer-macros [deftest is testing]]
            [malli-ts.core               :as-alias mts]
            [malli-ts.data-mapping :as sut]
            [malli.core :as m]
            [cljs-bean.core :as b]))

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
                                                             :TESTDummyXYZ "TD-B2"}}]}
                            :modelType)]
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
  (let [item-count 20
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
        clj-maps   (sut/to-clj *registry js-objs "modelType")]
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

(deftest test-a-clj-map-to-js
  (let [js-obj (sut/to-js *registry
                          {:schema-type  ::order
                           :order-id     "a-test-id-1234"
                           :order-type   "a-test-wf-type"
                           :total-amount 23456.89
                           :user         {:user-id "MrTesty"
                                          :name    "Testy The QA"}
                           :order-items  [{:order-item
                                           {:type          "some-test-order-item-type-1"
                                            :price         {:currency :EUR
                                                            :amount   22.3}
                                            :test-dummy    "TD-A1"
                                            :related-items [{:how-is-related "Dunno"}]}}
                                          {:order-item
                                           {:type       "some-test-order-item-type-2"
                                            :price      {:currency :ZAR
                                                         :amount   898}
                                            :test-dummy "TD-B2"}}]})]
    (testing "`to-js` should map a string"
      (is (= "a-test-id-1234" (-> js-obj .-orderId))))
    (testing "`to-js` should map a number"
      (is (= 23456.89 (-> js-obj .-totalAmount))))
    (testing "`to-js` should map a value from a nested map"
      (is (= "MrTesty" (-> js-obj .-user .-userId))))
    (testing "`to-js` should map a value from a nested vector"
      (is (= :EUR (-> js-obj .-orderItems (aget 0) .-orderItem .-price .-currency)))
      (is (= :ZAR (-> js-obj .-orderItems (aget 1) .-orderItem .-price .-currency))))
    (testing "`to-js` should map a value to a property with a different name"
      (is (= "TD-B2" (-> js-obj .-orderItems (aget 1) .-orderItem  .-TESTDummyXYZ))))
    (testing "`to-js` should map a value from a nested vector in a nested vector"
      (is (= "Dunno" (-> js-obj .-orderItems (aget 0) .-orderItem .-relatedItems (aget 0) .-howIsRelated))))))

(deftest test-clj-maps-to-js
  (let [item-count 20
        clj-maps   (->> item-count
                        range
                        (mapv
                         (fn [i]
                           {:schema-type  ::order
                            :order-id     (str "a-test-id-" i)
                            :order-type   (str "a-test-wf-type" i)
                            :total-amount (rand-amount)
                            :user         {:user-id (str "MrTesty" i)
                                           :name    (str "Testy The QA" i)}
                            :order-items  [{:order-item
                                            {:type          (str "some-test-order-item-type-A" i)
                                             :price         {:currency :EUR
                                                             :amount   (rand-amount)}
                                             :test-dummy    (str "TD-A" i)
                                             :related-items [{:how-is-related (str "Dunno" i)}]}}
                                           {:order-item
                                            {:type       (str "some-test-order-item-type-B" i)
                                             :price      {:currency :ZAR
                                                          :amount   (rand-amount)}
                                             :test-dummy (str "TD-B" i) }}]})))
        js-objs    (sut/to-js *registry clj-maps)]
    (doall (keep-indexed
            (fn [i js-obj]
              (testing "`to-js` given a vector, should map a string"
                (is (= (str "a-test-id-" i)
                       (-> js-obj .-orderId))))
              #_(testing "`to-clj` given an array, should map a number"
                (is (number? (:total-amount clj-map))))
              #_(testing "`to-clj` given an array, should map a value from a nested object"
                (is (= (str "MrTesty" i) (get-in clj-map [:user :user-id]))))
              #_(testing "`to-clj` given an array, should map a value from a nested array"
                (is (= :EUR (get-in clj-map [:order-items 0 :order-item :price :currency])))
                (is (= :ZAR (get-in clj-map [:order-items 1 :order-item :price :currency]))))
              #_(testing "`to-clj` given an array, should map a value from a property with a different name"
                (is (= (str "TD-B" i) (get-in clj-map [:order-items 1 :order-item :test-dummy]))))
              #_(testing "`to-clj` given an array, should map a value from a nested array in a nested array"
                (is (= (str "Dunno" i) (get-in clj-map [:order-items 0 :order-item :related-items 0 :how-is-related])))))
            js-objs))))

(comment
  (t/run-tests 'malli-ts.data-mapping-test)
  (t/test-vars [#'malli-ts.data-mapping-test/test-clj-maps-to-js])

  (require '[cljs-bean.core :as b])

  (let [item-count 20
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
                       (sut/into-js-array (range item-count)))]
    (b/bean js-objs))
  )
