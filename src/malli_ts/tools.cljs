(ns malli-ts.tools
  (:require [malli.core :as m]
            [malli.clj-kondo :as mc]
            [malli.registry :as mr]
            ["fs" :as fs]))

(def *registry-db
  (atom (m/default-schemas)))

(mr/set-default-registry!
 (mr/mutable-registry *registry-db))

(defn- n-letter [n]
  (let [alphabet "abcdefghijklmnopqrstuvwxyz"]
    (aget alphabet n)))

(defn- collect-lib [ns-sym]
  (-> (mc/collect ns-sym)))

(defn- underscorefy [fn-name]
  (clojure.string/replace (str fn-name) #"-" "_"))

(defn- to-ts-str-type [k-type]
  (clojure.core/name k-type))

(defn- collected-fn-to-ts-export [{:keys [name arity args ret] :as collected-fn}]
  (let [fn-name name
        args-name (map n-letter (range arity))
        joined-args (->>
                     (map (fn [arg-name arg-type] (str arg-name  ": " (to-ts-str-type arg-type)))
                          args-name args)
                     (clojure.string/join ", "))
        ret-type-str (to-ts-str-type ret)]

    (str "export function " (underscorefy fn-name) " (" joined-args ")"
         ": " ret-type-str ";")))

(comment
  (collected-fn-to-ts-export '{:ns malli-ts.lib,
                               :name greet,
                               :arity 2,
                               :args [:string :string],
                               :ret :string}))

(defn export-lib-str [ns-sym]
  (->> (collect-lib ns-sym)
       (map collected-fn-to-ts-export)
       (clojure.string/join "\n")))

(defn write-export [where export-str]
  (fs/writeFile where export-str identity))

