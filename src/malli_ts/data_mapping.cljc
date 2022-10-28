(ns malli-ts.data-mapping
  #?(:clj
     (:require
      [camel-snake-kebab.core :as csk]
      [clojure.set                 :as set]
      [malli-ts.core               :as-alias mts]
      [malli.core                  :as m]
      [malli.util                  :as mu]
      [mn.flow.domain.core.schemas :as schemas])
     :cljs
     (:require
      [camel-snake-kebab.core :as csk]
      [cljs-bean.core              :refer [bean bean? ->js ->clj]]
      [clojure.set                 :as set]
      [malli-ts.core               :as-alias mts]
      [malli.core                  :as m]
      [malli.util                  :as mu])))

(defn ancestors-path
  [clj->js-paths clj-path]
  (loop [js-path          []
         clj-path-segment clj-path]
    (let [path      (-> clj-path-segment
                        butlast
                        vec)
          last-step (peek path)]
      (if-not last-step
        (-> js-path
            reverse
            vec)
        (if (= ::m/in last-step)
          (recur
           (conj js-path last-step)
           path)
          (recur
           (conj js-path (get clj->js-paths path))
           path))))))

(defn get-schema
  [*schema-registry schema]
  (-> @*schema-registry
      schema
      (mu/get-in [0])))

(defn prop->key
  [js->clj-paths prop]
  (get js->clj-paths prop))

(defn key->prop
  [clj->js-paths key]
  (get clj->js-paths key))

#?(:cljs
   ;; TODO: Memoize/cache this
   (defn- clj<->js-key-mapping
     [*schema-registry model-type]
     (let [schema  (when model-type
                     (get-schema *schema-registry model-type))
           *result (atom [])]
       (when schema
         (m/walk
          schema
          (fn [schema path _children _options]
            (when (= :map (m/type schema))
              (let [clj-path->js-path (->> schema
                                           m/entries
                                           (mapcat #(let [key  (key %)
                                                          s    (-> %
                                                                   val
                                                                   m/schema)
                                                          prop (-> s
                                                                   m/properties
                                                                   ::mts/clj<->js
                                                                   :prop)
                                                          ;; TODO: Make this configurable
                                                          v    (or prop (csk/->camelCaseString key))]
                                                      (when v
                                                        [[(conj path key) v]
                                                         [key v]])))
                                           (remove nil?))]
                (when (seq clj-path->js-path)
                  (swap! *result concat clj-path->js-path))))
            schema))
         (let [clj->js-paths-keys (into {} @*result)
               clj->js-paths-keys (->> @*result
                                       (map (fn [[clj-path ts-prop]]
                                              (if (sequential? clj-path)
                                                (let [ap (ancestors-path
                                                          clj->js-paths-keys
                                                          clj-path)]
                                                  [clj-path (conj ap ts-prop)])
                                                [clj-path ts-prop])))
                                       (into {}))
               js->clj-paths-keys (set/map-invert clj->js-paths-keys)]
           {:clj->js-paths-keys clj->js-paths-keys
            :js->clj-paths-keys js->clj-paths-keys}))))

   ;; js/Proxy is a strange creature, neither `type`
   ;; nor `instance?` works for it, probably because
   ;; a Proxy doesn't have `Proxy.prototype` & has
   ;; transparent virtualization.
   (defprotocol IJsProxy)
   (deftype JsProxy []
     IJsProxy)

   (defn- map-bean
     [obj clj<->js-map]
     (when clj<->js-map
       (let [{:keys [clj->js-paths-keys
                     js->clj-paths-keys]}
             clj<->js-map
             fn-key->prop (partial key->prop clj->js-paths-keys)
             fn-prop->key (partial prop->key js->clj-paths-keys)]
         (bean obj :prop->key fn-prop->key :key->prop fn-key->prop :recursive true))))

   (declare to-clj')

   (defn- into-clj-vec
     [data clj<->js-map]
     (let [length (js/goog.object.get data "length" 0)]
       (-> (for [i (range length)]
             (do
               (-> data
                   (aget i)
                   (to-clj' clj<->js-map))))
           vec)))

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
     [*schema-registry data]
     (let [obj          (if (and (array? data)
                                 (>= (js/goog.object.get data "length" 0) 1))
                          (aget data 0)
                          data)
           model-type   (js/goog.object.get obj "modelType" nil)
           clj<->js-map (clj<->js-key-mapping *schema-registry model-type)]
       (to-clj' data clj<->js-map)))

   (declare map-proxy)

   (defn- array-push
     ([res] res)
     ([res x] (doto res (.push x))))

   (defn into-js-array
     [xform from]
     (transduce xform array-push (array) from))

   (defn- to-js'
     [data js->clj-paths-keys]
     (when js->clj-paths-keys
       (cond
         (or (sequential? data)
             (set? data))
         (into-js-array (map #(to-js' % js->clj-paths-keys)) data)

         (associative? data)
         (map-proxy data js->clj-paths-keys)

         :else
         data)))

   ;; TODO: Make model-type configurable
   (defn ^:export to-js
     [*schema-registry data]
     (let [is-coll    (or (sequential? data)
                          (set? data))
           model-type (if is-coll
                        (-> data
                            first
                            :model-type)
                        (:model-type data))
           {:keys [js->clj-paths-keys]
            :as   clj<->js-map}
           (clj<->js-key-mapping *schema-registry model-type)]
       (to-js' data js->clj-paths-keys)))

   (defn- map-proxy-get
     [js->clj-paths-keys target key]
     (case key
       "unwrap/clj" target

       (-> js->clj-paths-keys
           (get key)
           (as-> k (get target k))
           (to-js' js->clj-paths-keys))))

   (defn- map-proxy
     [data js->clj-paths-keys]
     (if (instance? JsProxy data)
       data
       (js/Proxy. data
                  #js
                  {:get            (partial map-proxy-get js->clj-paths-keys)
                   :getPrototypeOf (fn [k]
                                     (.-prototype JsProxy))})))

   (comment
     (let [next-act-schema [:vector
                            [:map
                             [:activity {:optional      true
                                         ::mts/clj<->js {:prop    "activity"
                                                         :fn-to   nil
                                                         :fn-from nil}}
                              [:map
                               [:type {::mts/clj<->js {:prop "type"}}
                                string?]
                               [:args any?]
                               [:test-dummy {::mts/clj<->js {:prop "testDummy"}}
                                string?]]]
                             [:sleep {:optional true}
                              [:map
                               [:timespan [:enum :milliseconds :seconds :minutes :hours :days]]
                               [:length int?]]]]]
           wf-exec-schema  [:map
                            [:model-type [:= ::workflow-exec]]
                            [:workflow-exec-id {::mts/clj<->js {:prop    "workflowExecId"
                                                                :fn-to   nil
                                                                :fn-from nil}}
                             string?]
                            [:workflow-type {::mts/clj<->js {:prop    "workflowType"
                                                             :fn-to   nil
                                                             :fn-from nil}}
                             [:or keyword? string?]]
                            [:next-activities {:optional      true
                                               ::mts/clj<->js {:prop "nextActivities"}}
                             next-act-schema]
                            [:state {:optional true}
                             any?]
                            [:user-id {:optional true}
                             string?]
                            [:events map?]
                            [:event-log any?]]
           s               [:schema {::mts/t-name  "WorkflowExec"
                                     ::mts/declare true}
                            wf-exec-schema]
           *registry       (atom (m/default-schemas))
           _               (swap! *registry assoc ::workflow-exec (m/schema s))
           clj-map         (to-clj *registry
                                   #js [#js
                                        {:modelType      ::workflow-exec
                                         :workflowExecId "a-test-id-1234"
                                         :workflowType   "a-test-wf-type"
                                         :nextActivities #js
                                         [#js
                                          {:activity #js
                                           {:type      "some-test-activity-type-1"
                                            :args      {:arg1 "dddd"
                                                        :arg2 22.3}
                                            :testDummy "dhjhjhdjhjd"}}]}])
           js-obj          (to-js *registry
                                  {:model-type       ::workflow-exec
                                   :workflow-exec-id "a-test-id-1234"
                                   :workflow-type    "a-test-wf-type"
                                   :next-activities  [{:activity
                                                       {:type       "some-test-activity-type-1"
                                                        :args       {:arg1 "dddd"
                                                                     :arg2 22.3}
                                                        :test-dummy "dhjhjhdjhjd"}}]})]
       #_(get-in clj-map [0 :next-activities 0 :activity :test-dummy])
       clj-map
       #_(-> js-obj
           .-nextActivities
           (aget 0)
           .-activity
           .-testDummy))


     )


   )
