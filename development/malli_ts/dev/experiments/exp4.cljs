(ns malli-ts.dev.experiments.exp4
  (:require [malli-ts.export.parse :refer [parse]]
            [malli-ts.export.transform :refer [transform]]
            [malli-ts.dev.exp1 :as exp1]
            [malli.core :as m]
            [malli.registry :as mr]
            ["fs" :as fs]))

(comment
  (exp1/register-schema!
   :flow/person
   [:map [:name :string] [:age :int]])

  (exp1/register-schema!
   :flow/address string?)

  (exp1/register-schema!
   :flow/company
   [:map
    [:name :string]
    [:people [:set :flow/person]]
    [:stuff [:set any?]]
    [:address :flow/address]])

  (m/validate :flow/person {:name "tiago" :age 21})

  (m/validate
   :flow/company
   {:name "Spurious Efforts"
    :people #{{:name "tiago" :age 21}}
    :address "Remote Island"})

  (let [file->content
        (parse
         [[:flow/person {:name "FlowPerson"
                         :file "flow/index.d.ts"}]
          [:flow/address {:name "FlowAddress"
                          :file "flow/company/index.d.ts"}]
          [:flow/company {:name "FlowCompany"
                          :file "flow/company/index.d.ts"}]
          [:account {:name "Account"
                     :file "flow/index.d.ts"}]] 
         {:export-default true
          :default-to-camel-case true
          :files-import-alias {"flow/index.d.ts" "flow"
                               "flow/company/index.d.ts" "fCompany"}
          :registry
          (merge
           @exp1/registry-db*
           {:account (m/schema [:map [:balance float?] [:asdf-asdf any?]])})})]
    
    (doseq [[file content] file->content]
      (println
       (str "-- "file " --" \newline
            content \newline)))))

(comment
  (prn (malli-ts.export.transform/transform :flow/company))
  :=> 
  {:$ref :flow/company,
   :definitions
   #:flow{:person
          {:type :object,
           :properties {:name {:type :string}, :age {:type :number}}},
          :company
          {:type :object,
           :properties
           {:name {:type :string},
            :people {:type :array, :items {:$ref :flow/person}}}}}})

(comment
  (transform :person {:registry {:person (m/schema [:map [:name string?]])}})

  (m/validate [:map ["asdfAsdf" int?]] {"asdfAsdf" 12}))

