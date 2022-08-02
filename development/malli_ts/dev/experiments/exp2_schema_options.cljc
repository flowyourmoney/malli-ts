(ns malli-ts.dev.experiments.exp2-schema-options
  (:require [malli-ts.core :refer [parse-files] :as mt]
            [malli.core :as m]
            [malli.util :as mu]))

(defn print-files
  [file->content]
  (doseq [[file content] file->content]
    (println

     (str "-- " file " --" \newline
          content \newline))))

(def flow-person-schema
  (m/schema [:map
             {::mt/t-name "Person" ::mt/export true ::mt/jsdoc [::mt/schema]}
             [:name string?] [:age pos-int?]]))

(def flow-office-schema
  (m/schema
   [:set
    {::mt/t-name "Office" ::mt/jsdoc [::mt/schema]}
    [:schema {:registry {:flow/person flow-person-schema}} :flow/person]]))

(def registry
  {:flow/person flow-person-schema
   :flow/office flow-office-schema
   :ibanxs/blah (m/schema [:enum {::mt/t-name "Blah"} "blah"])})

(comment
  (-> (parse-files
       {"flow/index.d.ts" [[:flow/person {}]]}
       {:export-default true
        :files-import-alias {"flow/index.d.ts" "flow"}
        :registry registry})
      print-files)

  (-> (mt/parse-matching-schemas {:registry registry})
      print-files)

  (-> (mt/parse-ns-schemas '(flow ibanxs) {:registry registry})
      print-files)

  (mu/update-properties
   (m/deref :flow/person {:registry registry}) assoc ::mt/t-name "FlowPersonV2"))
