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

(defn- deref-schema [schema defs]
  (if-let [ref (::ref schema)]
    (-> defs ref (deref-schema defs))
    schema))

(defn- walk-schema->clj<>js-mapping
  ([schema {::keys [prop-name-fn] :as options}]
   (m/walk
    schema
    (fn [schema' path children {::keys [*definitions] :as opts}]
      (let [s-type (m/type schema')]
        (case s-type
          :ref
          , {::ref (m/-ref schema')}

          (::m/schema :union :merge :select-keys)
          , (let [result (walk-schema->clj<>js-mapping (m/deref schema') opts)]
              (if-let [ref (m/-ref schema')]
                (do
                  (swap! *definitions assoc! ref result)
                  {::ref ref})
                result))

          (:schema :set :sequential :vector)
          , (first children)

          :enum
          , (m/form schema')

          :map-of
          , (Mapping. nil nil (second children))

          :or
          , (let [merged (->> children
                              (reduce
                               (fn [a b]
                                 (let [a (deref-schema a @*definitions)
                                       b (deref-schema b @*definitions)]
                                   (if (map? a) (if (map? b) (merge a b) #_else a)
                                       #_else b)))))]
              ;; Either the children-mappings merged into a single map, or the first child
              merged)

          :map
          , (->> children
                 (reduce
                  (fn [x [k opts s]]
                    (let [p (-> opts ::mts/clj<->js :prop (or (prop-name-fn k)))
                          m (Mapping. k p (first s))]
                      (assoc! x, k m, p m)))
                  (transient {}))
                 (persistent!))

          ; else
          (cond
            (empty? path)
            , (first children)

            (primitive? s-type)
            , s-type

            :else
            , children))))
    options)))

(defn- -clj<>js-mapping
  ([schema]
   (-clj<>js-mapping schema {}))
  ([schema {:keys [default-to-camel-case] :as options}]
   (let [*defs (or (::*definitions options)
                   (atom (transient {})))
         options
         (merge options
                {::*definitions       *defs
                 ::prop-name-fn       (if default-to-camel-case csk/->camelCaseString #_else name)
                 ::m/walk-schema-refs true
                 ::m/walk-refs        true
                 ::m/walk-entry-vals  true})
         root  (walk-schema->clj<>js-mapping schema options)]
     (as-> @*defs mapping
       (assoc! mapping ::root root)
       (persistent! mapping)
       ;; Follow mapping ::refs to their final value:
       (update-vals mapping
                    #(loop [v %] (if-let [v' (::ref v)] (recur (mapping v')) #_else v)))))))

(def clj<->js-mapping (memoize -clj<>js-mapping))
