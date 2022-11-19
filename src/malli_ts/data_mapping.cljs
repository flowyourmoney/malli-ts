(ns malli-ts.data-mapping
  (:require
   [camel-snake-kebab.core :as csk]
   [malli-ts.core          :as-alias mts]
   [malli.core             :as m]
   [cljs-bean.core :as b :refer [bean?]]))

;; js/Proxy is a strange creature, neither `type`
;; nor `instance?` works for it, probably because
;; a Proxy doesn't have `Proxy.prototype` & has
;; transparent virtualization.
(defprotocol IJsProxy)
(deftype JsProxy []
  IJsProxy)

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

(defn unwrap [v]
  (cond
    (instance? JsProxy v) (js/goog.object.get v "unwrap/clj" nil)
    (bean? v) v
    :else nil))

(defrecord Mapping [key prop schema])

(defn- deref-schema [js<->clj-mapping s]
  (if-let [ref (::ref s)]
    (do ;;(prn 'deref-schema #_#_'(@js<->clj-mapping ref) (@js<->clj-mapping ref) 'ref ref)
      (@js<->clj-mapping ref)) s))

(deftype BeanContext [js<->clj-mapping mapping ^:mutable sub-cache]
  #_#_IFn
  (-invoke [_ k] (deref-schema js<->clj-mapping (mapping k)))

  Object
  (toString [coll]
    (pr-str* (dissoc coll :js<->clj-mapping)))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (print-map {:js<->clj-mapping (if (instance? Atom js<->clj-mapping)'*atom* '*map*) :mapping mapping :sub-cache sub-cache} pr-writer writer opts))

  b/BeanContext
  (keywords? [_] true)
  (key->prop [_ key']
             ;(prn 'key->prop 'mapping mapping 'key key')
             (let [s (mapping key')]
               ;(prn 'key->prop 'MAPPING-BY-KEY' s)
               (set! sub-cache s)
               (.-prop s)))
  (prop->key [_ prop]
             ;(prn 'prop->key 'mapping mapping 'prop prop)
             (let [s (mapping prop)]
               ;(prn 'prop->key 'MAPPING-BY-PROP s)
               (set! sub-cache s)
               (.-key s)))
  (transform [self v prop key' nth']
    ;(prn 'transform v prop key' (if nth' nth' :NO-nth) #_sub-cache mapping)
    (if-some [v (unwrap v)] v
    ;else
             (if-some [bean' (cond (object? v) true (array? v) false)]
               (let [mapping js<->clj-mapping
                    ;;  (if (instance? Atom js<->clj-mapping) (set! js<->clj-mapping @js<->clj-mapping) js<->clj-mapping)
                     bean-context
                     (deref-schema mapping (if nth' self (.-schema sub-cache)))]
                 (if bean'
                   (b/Bean. nil v bean-context true nil nil nil)
                   (b/ArrayVector. nil bean-context v nil)))
      ;else
               v))))

(defn- -clj<>js-mapping
  ([schema]
   (let [*defs (atom (transient {}))
         root  (-clj<>js-mapping schema {::*definitions       *defs
                                         ::m/walk-schema-refs true
                                         ::m/walk-refs        true
                                         ::m/walk-entry-vals  true})
         result (-> @*defs
                    (assoc! ::root root)
                    (persistent!))]
     ;(prn 'result result)
     (reset! *defs result)))

  ([schema options]
   (m/walk
    schema
    (fn [schema' path children {::keys [*definitions] :as opts}]
      (let [s-type (m/type schema')]
        (case s-type
          :ref
          , {::ref (m/-ref schema')}

          ::m/schema
          , (let [result (-clj<>js-mapping (m/deref schema') opts)]
              (if-let [ref (m/-ref schema')]
                (do
                  (swap! *definitions assoc! ref result)
                  {::ref ref})
                result))

          (:enum :or)
          , (m/form schema')

          ; else
          (cond
            #_#_(and (empty? path)
                 (= s-type :vector)#_
                 (map? (ffirst children)))
            , (do
                ;(prn 'VECTOR s-type (type (first children)) (first children))
                (first children)#_
                (->BeanContext *definitions (first children) nil))


            (and (= s-type :map)
                 (seq children))
            , (->BeanContext *definitions
                             (->> children
                                  (reduce
                                   (fn [x [k opts s]]
                                     ;(prn 'reduce x k s)
                                     (let [p (-> opts ::mts/clj<->js :prop (or (csk/->camelCaseString k)))
                                           m (Mapping. k p (first s))]
                                       (assoc! x, k m, p m)))
                                   (transient {}))
                                  (persistent!)) nil)

            (and (= s-type ::m/schema)
                 (sequential? (first children)))
            , (ffirst children)

            (empty? path)
            , (do
                ;(prn 'EMPTY-PATH 's-type s-type #_children)
                (first children))

            (primitive? s-type)
            , s-type

            :else
            , children))))
    options)))

(def clj<->js-mapping (memoize -clj<>js-mapping))

