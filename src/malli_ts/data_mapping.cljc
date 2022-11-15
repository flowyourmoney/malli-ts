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

(def malli-complex-types ['seqable?
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
  (->> malli-complex-types
       (some #(= x %))
       nil?))

(defn primitive? [x]
  (or (number? x)
      (string? x)
      (boolean? x)
      (nil? x)))

(defn clj<->js-mapping
  ([schema key-type]
   (let [*defs (atom {})
         root  (clj<->js-mapping schema key-type {::*definitions       *defs
                                                  ::m/walk-schema-refs true
                                                  ::m/walk-refs        true
                                                  ::m/walk-entry-vals  true})]
     (merge {::root root}
            @*defs)))
  ([schema key-type options]
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
          , (let [result (clj<->js-mapping (m/deref schema') key-type opts)]
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
                      [(if (= :prop key-type) p k)
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
                     js->clj-mapping]}
             clj<->js-map
             fn-key->prop (partial key->prop clj-keys->js-props)
             fn-prop->key (partial prop->key js->clj-mapping)]
         (bean obj :prop->key fn-prop->key :key->prop fn-key->prop :recursive true))))

   (defn- transform-bean
     [{:keys [js->clj-mapping
              clj->js-mapping]
       :as   mapping} cur-js->clj-mapping cur-clj->js-mapping x
      {prop :prop, prop :key, nth' :nth, :as _ctx}]
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
                                 , (get-in cur-clj->js-mapping [prop :schema])

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
           :else          x))))

   (declare to-clj')

   (defn- into-clj-vec
     [x clj<->js-map]
     (let [{:keys [clj-keys->js-props
                   js->clj-mapping]}
           clj<->js-map
           fn-key->prop (partial key->prop clj-keys->js-props)
           fn-prop->key (partial prop->key js->clj-mapping)]
       (#'b/->val x fn-prop->key fn-key->prop nil)))

   (defn- to-clj' [x js<->clj-mapping]
     (cond
       ;; TODO: Fix this
       #_#_ (array? x)
       , (into-clj-vec x js<->clj-mapping)

       (bean? x)
       , x

       (instance? JsProxy x)
       , (js/goog.object.get x "unwrap/clj" nil)

       (or (object? x) (array? x))
       , (transform-bean js<->clj-mapping nil nil x nil)

       :else
       x))

   (defn ^:export to-clj
     ([x schema]
      (let [mapping {:js->clj-mapping (clj<->js-mapping schema :prop)
                     :clj->js-mapping (clj<->js-mapping schema :key)}]
        (to-clj' x mapping)))
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
                                                                       :relatedItems #js [#js {:credit
                                                                                               {:amount 676.30}}]}}
                                                             #js {:orderItem
                                                                  #js {:type         "some-test-order-item-type-2"
                                                                       :price        #js {:currency :ZAR
                                                                                          :amount   898}
                                                                       :TESTDummyXYZ "TD-B2"}}]} s)]
       #_(cljs.pprint/pprint (clj<->js-mapping s :->clj))
       #_(cljs.pprint/pprint (m/form s))
       #_(-> clj-map :order/user :user/id)
       (get-in clj-map [:order/items 1 :order/item :order-item/price :order-item/currency])
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
     ([x js->clj-mapping]
      (to-js' x js->clj-mapping (::root js->clj-mapping)))
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
      (let [mapping (clj<->js-mapping schema :prop)]
        (to-js' x mapping)))
     ([x registry schema]
      (let [s (m/schema [:schema {:registry registry}
                         schema])]
        (to-js x s))))

   (defn- map-proxy-get
     [js->clj-mapping cur-js->clj-mapping target prop]
     (let [cur-js->clj-mapping (if-let [ref (get cur-js->clj-mapping ::ref)]
                                 (get js->clj-mapping ref)
                                 cur-js->clj-mapping)
           new-cur-js->clj-m   (get-in cur-js->clj-mapping [prop :schema])]
       (cljs.pprint/pprint {#_#_:*T* target
                            :*P*     prop
                            :*CM*    cur-js->clj-mapping
                            :*N-CM*  new-cur-js->clj-m})
       (case prop
         "unwrap/clj" target

         (-> cur-js->clj-mapping
             (get-in [prop :key])
             (as-> k (get target k))
             (to-js' js->clj-mapping new-cur-js->clj-m)))))

   (defn- map-proxy
     [x js->clj-mapping cur-js->clj-mapping]
     (if (instance? JsProxy x)
       x
       (js/Proxy. x
                  #js
                  {:get            (partial map-proxy-get js->clj-mapping cur-js->clj-mapping)
                   :getPrototypeOf (fn [k]
                                     (.-prototype JsProxy))})))

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
       #_(-> js-obj .-user .-userId)
       (-> js-obj .-orderItems (aget 1) .-orderItem .-price .-currency)
       )

  )

   )


