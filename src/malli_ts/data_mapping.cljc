(ns malli-ts.data-mapping
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.set            :as set]
   [malli-ts.core          :as-alias mts]
   [malli.core             :as m]
   [malli.util             :as mu])
  #?(:cljs
     (:require
      [cljs-bean.core :as b :refer [bean bean? ->js ->clj]])))

(defn get-schema
  [*schema-registry schema]
  (-> @*schema-registry
      schema
      (mu/get-in [0])))

(defn prop->key
  [js-props->clj-keys prop]
  (get js-props->clj-keys prop))

(defn key->prop
  [clj-keys->js-props key]
  (get clj-keys->js-props key))

(defn prop->key2
  [js-props->clj-keys prop]
  (println :prop->key prop)
  (get js-props->clj-keys prop))

(defn- key->prop2
  [current-mapping key']
  (println :key->prop key' (get-in current-mapping [key' :prop]))
  (cljs.pprint/pprint current-mapping)
  (get-in current-mapping [key' :prop]))

(def default-get-schema-name :schema)

(defn- clj<->js-key-mapping
  ([*registry schema-name]
   (let [schema (when schema-name
                  (get-schema *registry schema-name))]
     (clj<->js-key-mapping schema)))
  ([schema]
   (let [*result (atom [])]
     (when schema
       (m/walk
        schema
        (fn [schema _path _children _options]
          (when (= :map (m/type schema))
            (let [clj-keys->js-props (->> schema
                                          m/entries
                                          (map #(let [key  (key %)
                                                      s    (-> %
                                                               val
                                                               m/schema)
                                                      prop (-> s
                                                               m/properties
                                                               ::mts/clj<->js
                                                               :prop)
                                                      v    (or prop (csk/->camelCaseString key))]
                                                  (when v
                                                    [key v])))
                                          (remove nil?))]
              (when (seq clj-keys->js-props)
                (swap! *result concat clj-keys->js-props))))
          schema))
       (let [clj-keys->js-props (into {} @*result)
             js-props->clj-keys (set/map-invert clj-keys->js-props)]
         {:clj-keys->js-props clj-keys->js-props
          :js-props->clj-keys js-props->clj-keys})))))

(def complex-types ['seqable?
                    'indexed?
                    'map?
                    'vector?
                    'list?
                    'seq?
                    'set?
                    'empty?
                    'sequential?
                    'coll?
                    'associative?
                    ::m/val])

(defn malli-primitive? [x]
  (->> complex-types
       (some #(= x %))
       nil?))

(defn primitive? [x]
  (or (number? x)
      (string? x)
      (boolean? x)
      (nil? x)))

(defn clj<->js-mapping
  ([schema ->clj-or->js]
   (let [*defs (atom {})
         root  (clj<->js-mapping schema ->clj-or->js {::*definitions       *defs
                                                      ::m/walk-schema-refs true
                                                      ::m/walk-refs        true
                                                      ::m/walk-entry-vals  true})]
     (merge {::root root}
            @*defs)))
  ([schema ->clj-or->js options]
   (m/walk
    schema
    (fn [schema' path children {::keys [*definitions] :as opts}]
      #_(cljs.pprint/pprint {:*P*    path
                             :*R*    (when (= (m/type schema') :ref)
                                       {:ref (m/-ref schema')})
                             :*T*    (m/type schema')
                             :*PRIM* (primitive? (m/type schema'))
                             #_:*C*  children
                             #_:*S*  (m/form schema')})
      (let [s-type (m/type schema')]
        (cond ;; TODO: Should probably rewrite this as a `defmulti`
          (empty? path)
          , (first children)

          (= s-type :ref)
          , {::ref (m/-ref schema')}

          (= s-type ::m/schema)
          , (let [result (clj<->js-mapping (m/deref schema') ->clj-or->js opts)]
              (if-let [ref (m/-ref schema')]
                (do
                  (swap! *definitions assoc ref result)
                  {::ref ref})
                result))

          (and (= s-type :map)
               (seq children))
          , (into {}
                  (for [[k opts s] children]
                    (let [p (get-in opts [::mts/clj<->js :prop]
                                    (csk/->camelCaseString k))]
                      [(if (= :->clj ->clj-or->js) p k)
                       {:key    k
                        :prop   p
                        :schema (first s)}])))
          (and (= s-type ::m/schema)
               (sequential? (first children)))
          , (ffirst children)

          (#{:enum :or} s-type)
          , (m/form schema')

          (malli-primitive? s-type)
          , s-type

          :else
          , children)))
    options)))

(def ^:private clj<->js-key-mapping-cached (memoize clj<->js-key-mapping))

;; js/Proxy is a strange creature, neither `type`
;; nor `instance?` works for it, probably because
;; a Proxy doesn't have `Proxy.prototype` & has
;; transparent virtualization.
(defprotocol IJsProxy)
(deftype JsProxy []
  IJsProxy)

#?(:cljs
   (defn default-js-get-schema-name [obj]
     (let [sn (csk/->camelCaseString default-get-schema-name)]
       (js/goog.object.get obj sn nil)))

   (defn- map-bean
     [obj clj<->js-map]
     (when clj<->js-map
       (let [{:keys [clj-keys->js-props
                     js-props->clj-keys]}
             clj<->js-map
             fn-key->prop (partial key->prop clj-keys->js-props)
             fn-prop->key (partial prop->key js-props->clj-keys)]
         (bean obj :prop->key fn-prop->key :key->prop fn-key->prop :recursive true))))

   (defn- transform-bean
     [{:keys [js->clj-mapping
              clj->js-mapping]
       :as   mapping} cur-js->clj-mapping cur-clj->js-mapping x
      {prop :prop, key' :key, nth' :nth, :as _ctx}]
     (println ::transform-bean x)
     (println)
     (if (primitive? x)
       x
       (let [cur-js->clj-mapping (if-let [ref (get cur-js->clj-mapping ::ref)]
                                   (get js->clj-mapping ref)
                                   cur-js->clj-mapping)
             cur-clj->js-mapping (if-let [ref (get cur-clj->js-mapping ::ref)]
                                   (get clj->js-mapping ref)
                                   cur-clj->js-mapping)
             new-cur-js->clj-m   (cond
                                   nth'
                                   , cur-js->clj-mapping

                                   cur-js->clj-mapping
                                   , (get-in cur-js->clj-mapping [prop :schema])

                                   :else
                                   , (::root js->clj-mapping))
             new-cur-clj->js-m (cond
                                 nth'
                                 , cur-clj->js-mapping

                                 cur-clj->js-mapping
                                 , (get-in cur-clj->js-mapping [key' :schema])

                                 :else
                                 , (::root clj->js-mapping))

             _            (cljs.pprint/pprint {:*C*   _ctx
                                               :*CM*  cur-clj->js-mapping
                                               :*NCM* new-cur-clj->js-m})
             fn-key->prop (partial key->prop2 new-cur-clj->js-m)
             fn-prop->key (partial prop->key2 new-cur-js->clj-m)
             fn-transform (partial transform-bean mapping new-cur-js->clj-m new-cur-clj->js-m)]

         (cond
           (primitive? x) x
           (object? x)    (b/Bean. nil x fn-prop->key fn-key->prop fn-transform true nil nil nil)
           (array? x)     (b/ArrayVector. nil fn-prop->key fn-key->prop fn-transform x nil)
           :else          x)))
     )

   (declare to-clj')

   (defn- into-clj-vec
     [x clj<->js-map]
     (let [{:keys [clj-keys->js-props
                   js-props->clj-keys]}
           clj<->js-map
           fn-key->prop (partial key->prop clj-keys->js-props)
           fn-prop->key (partial prop->key js-props->clj-keys)]
       (#'b/->val x fn-prop->key fn-key->prop nil)))

   (defn- to-clj' [x js<->clj-mapping]
     (cond
       (array? x) ;; TODO: Fix this
       (into-clj-vec x js<->clj-mapping)

       (bean? x)
       x

       (instance? JsProxy x)
       (js/goog.object.get x "unwrap/clj" nil)

       (object? x)
       (transform-bean js<->clj-mapping nil nil x nil)

       :else
       x))

   (defn ^:export to-clj
     ([x schema]
      (let [mapping {:js->clj-mapping (clj<->js-mapping schema :->js)
                     :clj->js-mapping (clj<->js-mapping schema :-clj)}]
        (to-clj' x mapping)))
     ([x registry schema]
      (let [s (m/schema [:schema {:registry registry}
                         schema])])))

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
                                                                       :relatedItems #js [#js {:howIsRelated "Dunno"}]}}
                                                             #js {:orderItem
                                                                  #js {:type         "some-test-order-item-type-2"
                                                                       :price        #js {:currency :ZAR
                                                                                          :amount   898}
                                                                       :TESTDummyXYZ "TD-B2"}}]} s)]
       #_(cljs.pprint/pprint (clj<->js-mapping s :->js))
       #_(cljs.pprint/pprint (m/form s))
       #_(-> clj-map :order/user :user/id)
       (get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency])
       )





     (let [order-items-schema [:vector string?
                               #_[:map
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
                                     [:vector [:map
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
                               [:order/items ::order-items]
                               [:order/total-amount {:optional      true
                                                     ::mts/clj<->js {:prop "totalAmount"}}
                                number?]
                               [:order/user {::mts/clj<->js {:prop "user"}
                                             :optional      true}
                                [:map
                                 [:user/id {::mts/clj<->js {:prop "userId"}} string?]
                                 [:user/name {:optional true} string?]]]]
           s                  (m/schema [:schema {:registry {::order-items order-items-schema}}
                                         order-schema])


           {::root {:order/id   nil
                    :order/user {:user/id   nil
                                 :user/name nil}}}
           #_#_clj-map (to-clj #js {"modelType"  ::order
                                    "orderId"    "2763yughjbh333"
                                    "orderType"  "Sport Gear"
                                    "user"       #js {"userId" "u678672"
                                                      "name"   "Kosie"}
                                    "orderItems" #js [#js {:orderItem
                                                           #js {:type         "some-test-order-item-type-1"
                                                                :price        #js {:currency :EUR
                                                                                   :amount   22.3}
                                                                :TESTDummyXYZ "TD-A1"
                                                                :relatedItems #js [#js {:howIsRelated "Dunno"}]}}
                                                      #js {:orderItem
                                                           #js {:type         "some-test-order-item-type-2"
                                                                :price        #js {:currency :ZAR
                                                                                   :amount   898}
                                                                :TESTDummyXYZ "TD-B2"}}]} s)]
       (m/form s)
       #_(let [x (get clj-map :order/user)
               y (get-in clj-map [:order/items 0 :order/item])
               z (:user/name x)])

       #_(get-in clj-map [:order/items 0 :order/item :order-item/price :order-item/currency])
       #_(get-in clj-map [:order/user :user/id])
    #_(cljs.pprint/pprint (m/form (m/schema s)))
    #_(cljs.pprint/pprint (clj<->js-key-mapping2 (m/schema s)))
    )

  )

   (declare map-proxy)

   (defn- array-push
     ([res] res)
     ([res x] (doto res (.push x))))

   (defn into-js-array
     [xform from]
     (transduce xform array-push (array) from))

   (defn- to-js'
     [x js-props->clj-keys]
     (when js-props->clj-keys
       (cond
         (or (sequential? x)
             (set? x))
         (into-js-array (map #(to-js' % js-props->clj-keys)) x)

         (associative? x)
         (map-proxy x js-props->clj-keys)

         :else
         x)))

   (defn ^:export to-js
     [x & {:keys [registry get-schema-name]
           :as   schema}]
     (cond
       (m/schema? schema)
       , (let [{:keys [js-props->clj-keys]} (clj<->js-key-mapping-cached
                                             schema)]
           (to-js' x js-props->clj-keys))

       (and registry get-schema-name)
       , (let [is-coll   (or (sequential? x)
                             (set? x))
               schema-nm (if is-coll
                           (-> x
                               first
                               get-schema-name)
                           (get-schema-name x))
               {:keys [js-props->clj-keys]}
               (clj<->js-key-mapping-cached registry schema-nm)]
           (to-js' x js-props->clj-keys))

       :else
       , (to-js x :registry registry :get-schema-name default-get-schema-name)))

   (defn- map-proxy-get
     [js-props->clj-keys target key]
     (case key
       "unwrap/clj" target

       (-> js-props->clj-keys
           (get key)
           (as-> k (get target k))
           (to-js' js-props->clj-keys))))

   (defn- map-proxy
     [x js-props->clj-keys]
     (if (instance? JsProxy x)
       x
       (js/Proxy. x
                  #js
                  {:get            (partial map-proxy-get js-props->clj-keys)
                   :getPrototypeOf (fn [k]
                                     (.-prototype JsProxy))})))
   )
