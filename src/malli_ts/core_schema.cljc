(ns malli-ts.core-schema)

(comment
  (require '[malli.provider :as mp]))

(comment
  (prn (mp/provide [{"malli-ts.ts.d.ts" [[:k {:declare true}]
                                         [:sym {:declare true}]
                                         [:toClj {:declare true}]
                                         [:validate {:declare true}]
                                         [:answerUltimateQuestion {:declare true}]
                                         [:now {:t-name "now" :declare true}]
                                         [:toSha256 {:t-name "toSha256" :declare true}]]
                     "random/dir/universe.d.ts" [[:random.dir.universe/answer-to-everything {:t-name "answerToEverything"}]]}])))

(def schema-type-options
  [:map
   [:declare {:optional true} boolean?]
   [:t-name {:optional true} string?]
   [:export {:optional true} boolean?]])

(def schema-type-options-with-literals
  (conj schema-type-options [:literal string?] [:jsdoc-literal string?]))

(def file->schema-type-vectors
  [:map-of
   string?
   [:vector
    [:catn
     [:schema-id keyword?]
     [:schema-type-options ::schema-type-options]]]])

(def parse-files-options
  [:map
   [:export-default {:optional true, :default true} boolean?]
   [:jsdoc-default {:optional true} [:vector keyword?]]
   [:use-default-schemas {:optional true, :default true} boolean?]
   [:files-import-alias {:optional true} [:map-of string? string?]]
   [:registry {:optional true} [:map-of any? any?]]])

(def parse-files-return
  [:map-of string? string?])

(def schema-id->type-options
  [:map-of keyword? schema-type-options])

(def assoc-literals-options
  (conj parse-files-options
        [:schema-id->type-options ::schema-id->type-options]
        [:file-imports* some?]
        [:files-import-alias* some?]))

(def registry
  {::schema-type-options schema-type-options
   ::schema-type-options-with-literals schema-type-options-with-literals
   ::file->schema-type-vectors file->schema-type-vectors
   ::parse-files-options parse-files-options
   ::parse-files-return parse-files-return
   ::schema-id->type-options schema-id->type-options
   ::assoc-literals-options assoc-literals-options})
