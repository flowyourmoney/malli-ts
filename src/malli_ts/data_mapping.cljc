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
       (map-bean data clj<->js-map)

       :else
       data))

   (defn ^:export to-clj
     [data & {:keys [registry get-schema-name]
              :as   schema}]
     (cond
       (m/schema? schema)
       , (->> schema
              clj<->js-key-mapping-cached
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
