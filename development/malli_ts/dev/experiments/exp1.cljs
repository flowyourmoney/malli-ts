(ns malli-ts.dev.experiments.exp1
  (:require [malli-ts.core :refer [parse-files]]
            [malli.core :as m]))

(comment
  (parse-files
   {"flow/index.d.ts" [[:flow/person {:t-name "FlowPerson" :export true}]]}

   {:export-default true
    :files-import-alias {"flow/index.d.ts" "flow"}
    :registry {:flow/person (m/schema [:map [:name string?] [:age pos-int?]])}}))

(comment
  (let [file->content
        (parse-files
         {"flow/index.d.ts" [[:flow/person {:t-name "FlowPerson"}]
                             [:account {:t-name "Account"}]]
          "flow/company/index.d.ts" [[:flow/address {:t-name "FlowAddress"}]
                                     [:flow/company {:t-name "FlowCompany"}]]}
         {:export-default true
          :default-to-camel-case true
          :files-import-alias {"flow/index.d.ts" "flow"
                               "flow/company/index.d.ts" "fCompany"}
          :registry
          (let [registry* (atom (m/default-schemas))
                update-registry!
                (fn [schema-id schema]
                  (swap! registry* assoc schema-id
                         (m/schema schema {:registry @registry*})))]
            (update-registry! :flow/person [:map [:name :string] [:age :int]])
            (update-registry! :flow/address string?)

            (update-registry! :flow/company [:map
                                             [:name :string]
                                             [:people [:set :flow/person]]
                                             [:stuff [:set any?]]
                                             [:address :flow/address]])

            (update-registry! :account [:map [:balance float?] [:asdf-asdf any?]])
            @registry*)})]
    
    (doseq [[file content] file->content]
      (println

       (str "-- "file " --" \newline
            content \newline)))))

