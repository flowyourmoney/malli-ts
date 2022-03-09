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

(defn now [] (js/Date.))

(defn toSha256 [s]
  (-> (js/require "crypto")
      (.createHash "sha256")
      (.update s)))

(comment
  (-> "hello world" toSha256 (.digest "hex")))

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
                                  [:validate {:t-name "validate" :declare true}]
                                  [:now {:t-name "now" :declare true}]
                                  [:toSha256 {:t-name "toSha256" :declare true}]]}
             {:export-default true
              :jsdoc-default [::mt/schema]
              :use-default-registry true
              :registry
              {:k [:=> [:catn [:s string?]] any?]
               :sym [:=> [:catn [:s string?]] any?]
               :toClj [:=> [:catn [:o any?]] any?]
               :validate [:=> [:catn [:schema any?] [:val any?]] any?]

               :date (mt/external-type "Date")
               :now [:=> [:cat] :date]

               :crypto/hash (mt/external-type "Hash" "crypto" "crypto")
               :toSha256 [:=> [:catn [:s string?]] :crypto/hash]}})
            first)]
    (.writeFileSync fs "examples/malli-ts.d.ts" content)))
