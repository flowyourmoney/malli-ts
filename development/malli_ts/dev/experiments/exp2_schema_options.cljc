(ns malli-ts.dev.experiments.exp2-schema-options
  (:require [malli-ts.core :refer [parse-files] :as mt]
            [malli.core :as m]))

(defn print-files
  [file->content]
  (doseq [[file content] file->content]
    (println

     (str "-- "file " --" \newline
          content \newline))))

(comment
  (-> (parse-files
       {"flow/index.d.ts" [[:flow/person {}]]}

       {:export-default true
        :files-import-alias {"flow/index.d.ts" "flow"}
        :registry
        {:flow/person
         (m/schema [:map
                    {::mt/t-name "FlowPerson" ::mt/export true ::jsdoc [::mt/schema]}
                    [:name string?] [:age pos-int?]])}})
      print-files))

