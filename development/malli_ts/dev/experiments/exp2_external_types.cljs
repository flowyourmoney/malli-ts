(ns malli-ts.dev.experiments.exp2-external-types
  (:require [malli-ts.core :refer [parse-files] :as mt]
            [malli.core :as m]))

(defn external-type
  ([type-name type-path type-import-alias]
   [any? {:external-type {:t-name type-name
                          :t-path type-path
                          :t-alias type-import-alias}}])
  ([type-name type-path]
   (external-type type-name type-path nil)))

(defn print-files
  [file->content]
  (doseq [[file content] file->content]
    (println

     (str "-- "file " --" \newline
          content \newline))))

(comment
  (-> (parse-files
       {"flow/index.d.ts" [[:flow/person {:t-name "FlowPerson"
                                          :export true
                                          :jsdoc [::mt/schema]}]]}

       {:export-default true
        :files-import-alias {"flow/index.d.ts" "flow"}
        :registry {:flow/person (m/schema [:map [:name string?] [:age pos-int?]])}})
      print-files))

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
          :jsdoc-default [::mt/schema]
          :registry
          (let [registry* (atom (m/default-schemas))
                update-registry!
                (fn [schema-id schema]
                  (swap! registry* assoc schema-id
                         (m/schema schema {:registry @registry*})))]
            (update-registry! :lib/ding-dong
                              (external-type "DingDong" "lib-asdf"))
            (update-registry! :flow/person [:map
                                            [:name :string]
                                            [:age :int]
                                            [:ding-dong :lib/ding-dong]])
            (update-registry! :flow/address string?)

            (update-registry! :flow/company [:map
                                             [:name :string]
                                             [:people [:set :flow/person]]
                                             [:stuff [:set any?]]
                                             [:address :lib/ding-dong]])

            (update-registry! :account [:map [:balance float?] [:asdf-asdf any?]])
            @registry*)})]
    (print-files file->content)))

