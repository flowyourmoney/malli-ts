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

(defn- schema-type
  [{:keys [schema] :as _schema-mapping}]
  )

(defn- key->prop2
  [schema-mapping *schema key']
  (println :key->prop key')
  #_(cljs.pprint/pprint {::key->prop2 {:S @*schema}})
  (when-let [new-schema-mapping (if @*schema
                                  (get-in @*schema [:keys<->props key'])
                                  (get-in schema-mapping [::root :keys<->props key']))]
    (let [#_#__               (cljs.pprint/pprint {::key->prop2 {:NSM new-schema-mapping}})
          new-schema-type (-> new-schema-mapping :schema m/type)
          new-schema      (if (#{:vector :map} new-schema-type)
                            (get schema-mapping key')
                            new-schema-mapping)]
      (reset! *schema new-schema)
      (get new-schema-mapping :prop))))

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

(defn- clj<->js-key-mapping2
  ([*registry schema-name]
   (let [schema (when schema-name
                  (get-schema *registry schema-name))]
     (clj<->js-key-mapping schema)))
  ([schema]
   (let [*result (atom {})]
     (when schema
       (m/walk
        schema
        (fn [schema _path _children _options]
          (when (= :map (m/type schema))
            (let [path               (->> (cons ::root _path)
                                          reverse
                                          (remove #(or (number? %)
                                                       (= % ::m/in))))
                  schema-key         (first path)
                  parent-schema-key  (second path)
                  clj-keys->js-props (->> schema
                                          m/entries
                                          (mapcat #(let [k                (key %)
                                                         malli-val-schema (val %)
                                                         s                (-> malli-val-schema m/form last m/schema)
                                                         prop             (-> malli-val-schema
                                                                              m/properties
                                                                              ::mts/clj<->js
                                                                              :prop)
                                                         p                (or prop (csk/->camelCaseString k))]
                                                     (when p
                                                       [[p {:key    k
                                                            :type   (m/type s)
                                                            :schema s}]
                                                        [k {:prop   p
                                                            :type   (m/type s)
                                                            :schema s}]])))
                                          (remove nil?)
                                          (into {}))]
              (when (seq clj-keys->js-props)
                (swap! *result merge {schema-key {:parent       parent-schema-key
                                                  :schema       schema
                                                  :keys<->props clj-keys->js-props}}))))
          schema))
       @*result
       #_(->> @*result
            ;; Given a key, we have a mapping to go from clj->js.
            ;; Now add a mapping, given a property, to go from js->clj.
              (mapcat (fn [[k {:keys [parent] :as v} :as schema-mapping]]
                        (let [prop (if parent
                                     (get-in @*result [parent :keys<->props k :prop])
                                     (str ::root))]
                        [schema-mapping [prop v]])))
              (into {}))))))

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

(defn primitive? [x]
  (->> complex-types
       (some #(= x %))
       nil?))

(defn clj<->js-mapping
  ([schema]
   (let [*defs (atom {})
         root  (clj<->js-mapping schema {::*definitions       *defs
                                         ::m/walk-schema-refs true
                                         ::m/walk-refs        true
                                         ::m/walk-entry-vals  true})]
     (merge {::root root}
            @*defs)))
  ([schema options]
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
        (cond
          (empty? path)
          , (first children)

          (= s-type :ref)
          , {::ref (m/-ref schema')}

          (= s-type ::m/schema)
          , (let [result (clj<->js-mapping (m/deref schema') opts)]
              (if-let [ref (m/-ref schema')]
                (do
                  (swap! *definitions assoc ref result)
                  {::ref ref})
                result))

          (and (= s-type :map)
               (seq children))
          , (into {}
                  (for [[k opts s] children]
                    [(:prop opts (name k)) {:key    k
                                            :schema (first s)}]))
          (and (= s-type ::m/schema)
               (sequential? (first children)))
          , (ffirst children)

          (#{:enum :or} s-type)
          , (m/form schema')

          (primitive? s-type)
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

   (defn- map-bean2
     [obj clj<->js-map *schema]
     (let [fn-key->prop (partial key->prop2 clj<->js-map *schema)
           fn-prop->key (partial prop->key2 clj<->js-map)
           fn-transform (partial transform clj<->js-map)]
       (bean obj
             :prop->key fn-prop->key
             :key->prop fn-key->prop
             :transform fn-transform
             :recursive true)))

   (defn- transform
     [clj<->js obj key']

     (let [schema (key' clj<->js)]
       (cond
         ;; if map/obj
         (map-bean2 obj clj<->js (atom @*schema-mapping))

         ;; if array

         ;; ...
         ))

     #_(cljs.pprint/pprint {::transform #_@*schema-mapping
                            (-> @*schema-mapping :schema m/form)}))

   (declare to-clj')

   (defn- into-clj-vec
     [data clj<->js-map]
     (let [{:keys [clj-keys->js-props
                   js-props->clj-keys]}
           clj<->js-map
           fn-key->prop (partial key->prop clj-keys->js-props)
           fn-prop->key (partial prop->key js-props->clj-keys)]
       (#'b/->val data fn-prop->key fn-key->prop nil)))

   (defn- to-clj' [data clj<->js-map]
     (cond
       (array? data)
       (into-clj-vec data clj<->js-map)

       (bean? data)
       data

       (instance? JsProxy data)
       (js/goog.object.get data "unwrap/clj" nil)

       (object? data)
       (map-bean2 data clj<->js-map (atom nil))

       :else
       data))

   (defn ^:export to-clj
     [data & {:keys [registry get-schema-name]
              :as   schema}]
     (cond
       (m/schema? schema)
       , (->> schema
              clj<->js-key-mapping2
              (to-clj' data))

       (and registry get-schema-name)
       , (let [get-schema-name (if (fn? get-schema-name)
                                 get-schema-name
                                 (fn to-clj-get-schema-nm [o]
                                   (let [schema-nm (if (keyword? get-schema-name)
                                                     (name get-schema-name)
                                                     (str get-schema-name))]
                                     (js/goog.object.get o schema-nm nil))))
               obj             (if (and (array? data)
                                        (>= (js/goog.object.get data "length" 0) 1))
                                 (aget data 0)
                                 data)
               clj<->js-map    (clj<->js-key-mapping-cached registry
                                                            (get-schema-name obj))]
           (to-clj' data clj<->js-map))

       :else
       , (to-clj data :registry registry :get-schema-name default-js-get-schema-name)))

   (comment
     (-> (m/schema [:map [:x ::y]] {:registry (merge (m/default-schemas) {::y string?})}) m/form)


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
                               [:order/items ::order-items]
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
                                         order-schema])]
       (cljs.pprint/pprint (clj<->js-mapping s))
       #_(cljs.pprint/pprint (m/form s))
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
     [data js-props->clj-keys]
     (when js-props->clj-keys
       (cond
         (or (sequential? data)
             (set? data))
         (into-js-array (map #(to-js' % js-props->clj-keys)) data)

         (associative? data)
         (map-proxy data js-props->clj-keys)

         :else
         data)))

   (defn ^:export to-js
     [data & {:keys [registry get-schema-name]
              :as   schema}]
     (cond
       (m/schema? schema)
       , (let [{:keys [js-props->clj-keys]} (clj<->js-key-mapping-cached
                                             schema)]
           (to-js' data js-props->clj-keys))

       (and registry get-schema-name)
       , (let [is-coll   (or (sequential? data)
                             (set? data))
               schema-nm (if is-coll
                           (-> data
                               first
                               get-schema-name)
                           (get-schema-name data))
               {:keys [js-props->clj-keys]}
               (clj<->js-key-mapping-cached registry schema-nm)]
           (to-js' data js-props->clj-keys))

       :else
       , (to-js data :registry registry :get-schema-name default-get-schema-name)))

   (defn- map-proxy-get
     [js-props->clj-keys target key]
     (case key
       "unwrap/clj" target

       (-> js-props->clj-keys
           (get key)
           (as-> k (get target k))
           (to-js' js-props->clj-keys))))

   (defn- map-proxy
     [data js-props->clj-keys]
     (if (instance? JsProxy data)
       data
       (js/Proxy. data
                  #js
                  {:get            (partial map-proxy-get js-props->clj-keys)
                   :getPrototypeOf (fn [k]
                                     (.-prototype JsProxy))})))
   )
