(ns malli-ts.data-mapping
  (:require
   [camel-snake-kebab.core :as csk]
   [malli-ts.core          :as-alias mts]
   [malli.core             :as m]))

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

          (primitive? s-type)
          , s-type

          :else
          , children)))
    options)))

;; js/Proxy is a strange creature, neither `type`
;; nor `instance?` works for it, probably because
;; a Proxy doesn't have `Proxy.prototype` & has
;; transparent virtualization.
(defprotocol IJsProxy)
(deftype JsProxy []
  IJsProxy)
