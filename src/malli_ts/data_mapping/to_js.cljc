(ns malli-ts.data-mapping.to-js
  (:require
   [camel-snake-kebab.core :as csk]
   [malli-ts.core          :as-alias mts]
   [malli-ts.data-mapping  :as mts-dm]
   [malli.core             :as m]))

(declare map-proxy)

#?(:cljs
   (defn- array-push
     ([res] res)
     ([res x] (doto res (.push x))))

   (defn into-js-array
     [xform from]
     (transduce xform array-push (array) from))

   (defn- to-js'
     ([x js->clj-mapping]
      (to-js' x js->clj-mapping (::mts-dm/root js->clj-mapping)))
     ([x js->clj-mapping cur-js->clj-mapping]
      (cond
        (or (sequential? x)
            (set? x))
        , (into-js-array (map #(to-js' %  js->clj-mapping cur-js->clj-mapping)) x)

        (associative? x)
        , (map-proxy x js->clj-mapping cur-js->clj-mapping)

        :else
        , x)))

   (defn ^:export to-js
     ([x schema]
      (let [mapping (mts-dm/clj<->js-mapping schema :prop)]
        (to-js' x mapping)))
     ([x registry schema]
      (let [s (m/schema [:schema {:registry registry}
                         schema])]
        (to-js x s))))

   (defn- map-proxy-get
     [js->clj-mapping cur-js->clj-mapping target prop]
     (let [cur-js->clj-mapping (if-let [ref (get cur-js->clj-mapping ::mts-dm/ref)]
                                 (get js->clj-mapping ref)
                                 cur-js->clj-mapping)
           new-cur-js->clj-m   (get-in cur-js->clj-mapping [prop :schema])]
       (case prop
         "unwrap/clj" target

         (-> cur-js->clj-mapping
             (get-in [prop :key])
             (as-> k (get target k))
             (to-js' js->clj-mapping new-cur-js->clj-m)))))

   (defn- map-proxy
     [x js->clj-mapping cur-js->clj-mapping]
     (if (instance? mts-dm/JsProxy x)
       x
       (js/Proxy. x
                  #js
                  {:get            (partial map-proxy-get js->clj-mapping cur-js->clj-mapping)
                   :getPrototypeOf (fn [k]
                                     (.-prototype mts-dm/JsProxy))})))

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
       #_(-> js-obj .-orderItems (aget 1) .-orderItem .-price .-currency)
       )

  )

)