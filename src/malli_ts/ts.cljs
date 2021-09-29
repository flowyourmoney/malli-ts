(ns malli-ts.ts
  (:require [malli-ts.core :as mt]
            [malli.core :as m]
            [cljs-bean.core :as b]
            ["fs" :as fs]))

(defn k [s] (keyword s))
(defn sym [s] (symbol s))

(defn bean [o] (b/bean o))

(defn toClj [o] (b/->clj o))

(comment
  (m/validate
   [:map [:a int?]]
   (toClj #js {"a" 1})))

(defn validate
  [& args]
  (apply m/validate (map b/->clj args)))

(defn exports-fn
  []
  (comment
    (-> (doto (ns-publics 'malli-ts.ts) prn)
        (dissoc 'exports-fn)
        (->> (map (fn [[sym var]] [(keyword sym) @var]))
             (into {}))
        clj->js))
  #js {:k k
       :sym sym
       :bean bean
       :toClj toClj
       :validate validate})

(defn gen-types
  [& args]
  (let [[_ content]
        (-> (mt/parse-files
             {"malli-ts.ts.d.ts" [[:k {:t-name "k" :declare true}]
                                  [:sym {:t-name "sym" :declare true}]
                                  [:toClj {:t-name "toClj" :declare true}]
                                  [:validate {:t-name "validate" :declare true}]]}
             {:export-default true
              :jsdoc-default [::mt/schema]
              :registry
              {:k (m/schema [:=> [:catn [:s string?]] any?])
               :sym (m/schema [:=> [:catn [:s string?]] any?])
               :toClj (m/schema [:=> [:catn [:o any?]] any?])
               :validate (m/schema [:=> [:catn [:schema any?] [:val any?]] any?])}})
            first)]
    (.writeFileSync fs "node_modules/malli-ts.ts.d.ts" content)))
