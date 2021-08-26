(ns malli-ts.core
  (:require [malli.core :as m]
            [camel-snake-kebab.core :as csk]))

(defmulti ->type-literal-str
  (fn [schema _]
    (first schema)))

(defn -get-type-literal
  [typedef-registry s-form]
  (let [another-schema? (vector? s-form)
        type-literal (if another-schema?
                       (->type-literal-str s-form typedef-registry)
                       (or (-get-type-literal typedef-registry s-form)
                           "any"))
        typedef (get typedef-registry schema-id)]
    (or (:type-name typedef) (:literal typedef))))

(defmethod ->type-literal-str :map
  [schema typedef-registry]
  (let [entry-literals
        (for [[k opts child] (m/children schema)
              :let [entry-name (csk/->camelCaseString k)
                    s-form (m/-form child)
                    type-literal (-get-type-literal typedef-registry s-form)
                    optional? (:optional opts)]]
          (str entry-name (when optional? "?") ":" type-literal))]
    (str "{"(clojure.string/join "," entry-literals) "}")))

(comment
  (->type-literal-str   
   [:map
    [:a :number]
    [:b :string]
    [:c :boolean]]
   {:number {:literal "number"}
    :string {:literal "string"}
    :boolean {:literal "boolean"}}))

(defmethod ->type-literal-str :vector
  [[_ s-form] typedef-registry]
  (let [another-schema-literal? (vector? s-form)
        type-literal (-get-type-literal typedef-registry s-form)]
    (str type-literal "[]")))

(comment
  (->type-literal-str [:vector :number] {:number {:literal "number"}}))

(defmethod ->type-literal-str :tuple
  [[_ & children] typedef-registry]
  (let [type-literals
        (for [s-form children
              :let [another-schema-literal? (vector? s-form)]]
          (-get-type-literal typedef-registry s-form))]
    (str "[" (clojure.string/join "," type-literals) "]")))

(comment
  (->type-literal-str   
   [:tuple :number :string :boolean]
   {:number {:literal "number"}
    :string {:literal "string"}
    :boolean {:literal "boolean"}}))

(defn ->type-declaration-str
  [type-name type-literal opts]
  (let [{:keys [export?]} opts]
    (str (if export? "export " nil)
         "type "  type-name " = " type-literal ";")))

