(ns malli-ts.dev.experiments.exp2-schema-parsing
  (:require [camel-snake-kebab.core :as csk]
            [malli-ts.dev.experiments.exp1 :as exp1]
            [malli.core :as m]
            [malli.registry :as mr]))

(comment (mr/set-default-registry!
          (mr/mutable-registry exp1/registry-db*)))

(defmulti type-literal-str
  (fn [schema _]
    (first schema)))

(defmethod type-literal-str :map
  [schema typedef-registry]
  (let [entry-literals
        (for [[k opts child] (m/children schema)
              :let [entry-name (csk/->camelCaseString k)
                    s-form (m/-form child)
                    another-schema? (coll? s-form)
                    type-literal (if another-schema?
                                   (type-literal-str s-form typedef-registry)
                                   (or (get-in typedef-registry [s-form :str])
                                       "any"))
                    optional? (:optional opts)]]
          (str entry-name (when optional? "?") ": " type-literal))]
    (str "{\n"(clojure.string/join ",\n" entry-literals) "\n}")))

(comment
  (exp1/register-schema! ::id number?))

(comment
  (type-literal-str   
   [:map
    ::id
    [:a :number]
    [:b :string]
    [:c :boolean]]
   {::id {:str "Id"}
    :number {:str "number"}
    :string {:str "string"}
    :boolean {:str "boolean"}}))

(comment (parse-map [:map
                     [:a :number]
                     [:b :string]]))

(comment
  (let [children (m/children [:map
                              [:a :int]
                              [:b :string]])
        schema (get-in children [0 2])]
    (m/-form schema)))
