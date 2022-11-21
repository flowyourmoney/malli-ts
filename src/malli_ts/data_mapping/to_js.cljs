(ns malli-ts.data-mapping.to-js
  (:require
   [malli-ts.core          :as-alias mts]
   [malli-ts.data-mapping  :as mts-dm]
   [malli.core             :as m]))

(declare to-js')

(deftype JsProxy [js->clj-mapping cur-js->clj-mapping]
  Object
  (get [this target prop]
    (case prop
      "unwrap/clj" target
      (or (unchecked-get this prop)
          (when-let [mapping (cur-js->clj-mapping prop)]
            (let [proxy (to-js' (target (.-key mapping)) js->clj-mapping (.-schema mapping))]
              ;; cache proxy as `this[prop]`
              (unchecked-set this prop proxy)
              proxy)))))
  (getPrototypeOf [this]
    (.-prototype this)))

(defn- to-js'
  ([x js->clj-mapping cur-js->clj-mapping]
   (cond
     ;; If the "unwrap/clj" property exists, x is already wrapped
     ;; Cast to boolean for unwrapped JS truthyness check
     ^boolean (unchecked-get x "unwrap/clj")
     , x

     (or (sequential? x)
         (set? x))
     , (let [len (count x)
             arr (js/Array. len)]
         (loop [i 0 x (seq x)]
           (if x
             (let [[v & rest] x]
               (unchecked-set arr i (to-js' v js->clj-mapping cur-js->clj-mapping))
               (recur (unchecked-inc i) rest))
             arr)))

     (associative? x)
     , (let [cur-js->clj-mapping
             (if-let [ref (::mts-dm/ref cur-js->clj-mapping)] (js->clj-mapping ref)
              #_else cur-js->clj-mapping)]
         (js/Proxy. x (JsProxy. js->clj-mapping cur-js->clj-mapping)))

     :else
     , x)))

(defn ^:export to-js
  ([x mapping]
   (to-js' x mapping (::mts-dm/root mapping)))
  ([x registry schema]
   (let [s (m/schema [:schema {:registry registry}
                      schema])]
     (to-js x (mts-dm/clj<->js-mapping s)))))

(comment
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
                                        [:ref ::order-items]
                                        #_[:vector [:map
                                                    [:related-item/how-is-related string?
                                                     :related-item/order-item-id uuid?]]]]]]
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
                            #_[:order/items {:optional      true
                                             ::mts/clj<->js {:prop "orderItems"}}
                               [:ref ::order-items]]
                            [:order/items {:optional      true
                                           ::mts/clj<->js {:prop "orderItems"}} ::order-items]
                            [:order/total-amount {:optional      true
                                                  ::mts/clj<->js {:prop "totalAmount"}}
                             number?]
                            [:order/user {::mts/clj<->js {:prop "user"}
                                          :optional      true}
                             [:map
                              [:user/id {::mts/clj<->js {:prop "userId"}} string?]
                              [:user/name {:optional true} string?]]]]
        r                  {::order-items order-items-schema}
        s                  (m/schema [:schema {:registry {::order-items order-items-schema}}
                                      order-schema])
        js-obj             (to-js  {:model-type  ::order
                                    :order/id    "2763yughjbh333"
                                    :order/type  "Sport Gear"
                                    :order/user  {:user/id   "u678672"
                                                  :user/name "Kosie"}
                                    :order/items [{:order/item
                                                   {:order-item/type          "some-test-order-item-type-1"
                                                    :order-item/price         {:currency :EUR
                                                                               :amount   22.3}
                                                    :order-item/test-dummy    "TD-A1"
                                                    :order-item/related-items [{:order/credit
                                                                                {:order.credit/amount 676.30}}]}}
                                                  {:order/item
                                                   {:order-item/type       "some-test-order-item-type-2"
                                                    :order-item/price      {:order-item/currency :ZAR
                                                                            :order-item/amount   898}
                                                    :order-item/test-dummy "TD-B2"}}]} s)]
    #_(cljs.pprint/pprint (clj<->js-mapping s :prop))
    (-> js-obj .-user .-userId)
    #_(-> js-obj .-orderItems (aget 1) .-orderItem .-price .-currency)))
