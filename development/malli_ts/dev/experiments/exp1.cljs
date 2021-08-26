(ns malli-ts.dev.experiments.exp1
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(defonce registry-db*
  (atom (merge (m/default-schemas)
               {:number (m/-simple-schema
                         {:type :number
                          :pred number?
                          :property-pred (m/-min-max-pred nil)})
                :string (m/-simple-schema
                         {:type :string
                          :pred string?})})))

(mr/set-default-registry!
 (mr/mutable-registry registry-db*))

(defn register-schema!
  [schema-id schema]
  (swap! registry-db* assoc schema-id schema))

(register-schema! :neg-number [:and number? neg?])

(comment (m/validate :neg-number -1))

(register-schema!
 :flow/person
 [:map [:name :string] [:account-balance :neg-number]])

(comment (m/validate :flow/person {:name "Tiago" :account-balance -100}))

(register-schema! :flow/family [:vector :flow/person])

(comment (m/validate :flow/family [{:name "Tiago" :account-balance -100}
                                   {:name "Danny" :account-balance -200}
                                   {:name "Niels" :account-balance -300}]))

