(ns malli-ts.dev.experiments.exp2-schema-options
  (:require [malli-ts.core :refer [parse-files] :as mt]
            [malli.core :as m]
            [malli.util :as mu]))

(defn print-files
  [file->content]
  (doseq [[file content] file->content]
    (println

     (str "-- "file " --" \newline
          content \newline))))

(def registry
  {:flow/person
   (m/schema [:map
              {::mt/t-name "FlowPerson" ::mt/export true ::mt/jsdoc [::mt/schema]}
              [:name string?] [:age pos-int?]])})

(comment
  (-> (parse-files
       {"flow/index.d.ts" [[:flow/person {}]]}
       {:export-default true
        :files-import-alias {"flow/index.d.ts" "flow"}
        :registry registry})
      print-files)
  
  (-> (mt/parse-ns-matching-schemas
       {:registry registry})
      print-files)

(mu/update-properties
 (m/deref :flow/person {:registry registry}) assoc ::mt/t-name "FlowPersonV2"))
