(ns malli-ts.dev.experiments.exp3
  (:require [malli-ts.export.core :as ts-export]
            [malli-ts.dev.exp1 :as exp1]
            [malli.core :as m]
            [malli.registry :as mr]
            ["fs" :as fs]))

(mr/set-default-registry!
 (mr/mutable-registry exp1/registry-db*))

(exp1/register-schema!
 :flow/person
 [:map
  [:name :string]
  [:age :number]])

(exp1/register-schema!
 :flow/family
 [:vector :flow/person])

(exp1/register-schema!
 :flow/company
 [:map
  [:owners [:vector :flow/person]]
  [:employees [:vector :flow/person]]
  [:nested-type-test
   [:map [:k1 :number] [:k2 :boolean]]]])

(comment
  (mr/-schema @mr/registry* :flow/person))

(def primitive-types-mapping
  {:number {:literal "number"}
   :literaling {:literal "string"}
   :boolean {:literal "boolean"}})

(def types-mapping
  (loop [schema-ids [[:flow/person "FlowPerson"]
                     [:flow/family "FlowFamily"]
                     [:flow/company "FlowCompany"]]
         types-mapping primitive-types-mapping]
    (let [[s-id t-name] (first schema-ids)]
      (if s-id
        (recur
         (rest schema-ids)
         (merge types-mapping
                {s-id {:type-name t-name
                       :literal (ts-export/->type-literal-str
                                 (mr/-schema @mr/registry* s-id)
                                 types-mapping)}}))
        types-mapping))))

(comment (prn types-mapping))

(comment
  (let [file-lines (map
                    (fn [s-id]
                      (let [{:keys [type-name str]} (-> types-mapping s-id)]
                        (ts-export/->type-declaration-str
                         type-name
                         str
                         {:export? true})))
                    [:flow/person :flow/family :flow/company])
        file-content (clojure.string/join "\n" file-lines)
        where "out/"]
    (fs/writeFile (str where "exp3.d.ts") file-content println)))


(comment
  (-> [:map [:a :number] [:b :string] [:c int?]]
      m/children
      (get-in [2 2])
      m/type))
