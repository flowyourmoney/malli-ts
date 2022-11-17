(ns malli-ts.data-mapping
  (:require
   [camel-snake-kebab.core :as csk]
   [malli-ts.core          :as-alias mts]
   [malli.core             :as m]))

(def complex-types #{'seqable?
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
                     ::m/val})

(defn primitive? [x]
  (nil? (complex-types x)))

(defrecord Mapping [key prop schema])

(defn- -clj<>js-mapping
  ([schema key-type]
   (let [*defs (atom {})
         root  (-clj<>js-mapping schema key-type {::*definitions       *defs
                                                  ::m/walk-schema-refs true
                                                  ::m/walk-refs        true
                                                  ::m/walk-entry-vals  true})]
     (merge {::root root}
            @*defs)))
  ([schema key-type options]
   (m/walk
    schema
    (fn [schema' path children {::keys [*definitions] :as opts}]
      (let [s-type (m/type schema')]
        (case s-type
          :ref
          , {::ref (m/-ref schema')}

          ::m/schema
          , (let [result (-clj<>js-mapping (m/deref schema') key-type opts)]
              (if-let [ref (m/-ref schema')]
                (do
                  (swap! *definitions assoc ref result)
                  {::ref ref})
                result))

          (:enum :or)
          , (m/form schema')

          ; else
          (cond
            (empty? path)
            , (first children)

            (and (= s-type :map)
                 (seq children))
            , (->> children
                   (map (fn [[k opts s]]
                          (let [p (-> opts ::mts/clj<->js :prop (or (csk/->camelCaseString k)))]
                            [(if (= :prop key-type) p k)
                             (Mapping. k p (first s))])))
                   (into {}))

            (and (= s-type ::m/schema)
                 (sequential? (first children)))
            , (ffirst children)

            (primitive? s-type)
            , s-type

            :else
            , children))))
    options)))

(def clj<->js-mapping (memoize -clj<>js-mapping))

;; js/Proxy is a strange creature, neither `type`
;; nor `instance?` works for it, probably because
;; a Proxy doesn't have `Proxy.prototype` & has
;; transparent virtualization.
(defprotocol IJsProxy)
(deftype JsProxy []
  IJsProxy)
