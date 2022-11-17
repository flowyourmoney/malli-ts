(ns malli-ts.data-mapping.to-clj
  (:require
   [malli-ts.core          :as-alias mts]
   [malli-ts.data-mapping  :as mts-dm]
   [malli.core             :as m]
   [cljs-bean.core :as b :refer [bean? primitive?]]))

(declare to-clj')

(defn- transform-bean
  [js->clj-mapping
   clj->js-mapping 
   cur-js->clj-mapping
   cur-clj->js-mapping
   x
   _ctx]

  (if (primitive? x)
    x
    
    (let [{prop :prop, key' :key, nth' :nth} _ctx
          cur-js->clj-mapping (if-let [ref (::mts-dm/ref cur-js->clj-mapping)]
                                (js->clj-mapping ref)
                                cur-js->clj-mapping)

          cur-clj->js-mapping (if-let [ref (::mts-dm/ref cur-clj->js-mapping)]
                                (clj->js-mapping ref)
                                cur-clj->js-mapping)

          new-cur-js->clj-m   (cond
                                nth'
                                , cur-js->clj-mapping
                                cur-js->clj-mapping
                                , (:schema (cur-js->clj-mapping prop))
                                :else
                                , (::mts-dm/root js->clj-mapping))

          new-cur-clj->js-m   (cond
                                nth'
                                , cur-clj->js-mapping
                                cur-clj->js-mapping
                                , (:schema (cur-clj->js-mapping key'))
                                :else
                                , (::mts-dm/root clj->js-mapping))

          fn-key->prop        (fn [key'] (-> new-cur-clj->js-m key' :prop))
          fn-prop->key        (fn [prop] (:key (get new-cur-js->clj-m prop)))
          fn-transform        (fn [v cx] (transform-bean js->clj-mapping clj->js-mapping new-cur-js->clj-m new-cur-clj->js-m v cx))]

      (cond
        (object? x)    (b/Bean. nil x fn-prop->key fn-key->prop fn-transform true nil nil nil)
        (array? x)     (b/ArrayVector. nil fn-prop->key fn-key->prop fn-transform x nil)
        :else          x))))

(defn- to-clj' [x js<->clj-mapping clj->js-mapping]
  (cond
    (bean? x)
    , x

    (instance? mts-dm/JsProxy x)
    , (js/goog.object.get x "unwrap/clj" nil)

    (or (object? x) (array? x))
    , (transform-bean js<->clj-mapping clj->js-mapping nil nil x nil)

    :else
    x))

(defn ^:export to-clj
  ([x schema] 
   (to-clj' x (mts-dm/clj<->js-mapping schema :prop) (mts-dm/clj<->js-mapping schema :key)))
  ([x registry schema]
   (let [s (m/schema [:schema {:registry registry}
                      schema])]
     (to-clj x s))))

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
                                       [:order-credit/amount number?]]]]]
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
        clj-map            (to-clj #js {"modelType"  ::order
                                        "orderId"    "2763yughjbh333"
                                        "orderType"  "Sport Gear"
                                        "user"       #js {"userId" "u678672"
                                                          "name"   "Kosie"}
                                        "orderItems" #js [#js {:orderItem
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
                                                                    :TESTDummyXYZ "TD-B2"}}]} s)]
    #_{:js->clj-mapping (mts-dm/clj<->js-mapping s :prop)
       :clj->js-mapping (mts-dm/clj<->js-mapping s :key)}
    #_(-> clj-map :order/user :user/id)
    (get-in clj-map [:order/items 0 :order/item :order-item/related-items 0
                     :order/credit :order-credit/amount])
    #_(get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency])))
