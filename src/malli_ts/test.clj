(ns malli-ts.test
  (:require [clojure.core :refer [spit]]
            [malli-ts.core :as mts]))

(def schemas
  {:k [:=> [:catn [:s string?]] any?]
   :sym [:=> [:catn [:s string?]] any?]
   :toClj [:=> [:catn [:o any?]] any?]
   :validate [:=> [:catn [:schema any?] [:val any?]] any?]
   :my-identity [:=> [:catn [:x any?]] any?]})

(defn prep-lib
  []
  (let [[_ content]
        (-> (mts/parse-files
             {"flow-domain.core.d.ts" [[:k {:t-name "k" :declare true}]
                                       [:sym {:t-name "sym" :declare true}]
                                       [:toClj {:t-name "toClj" :declare true}]
                                       [:validate {:t-name "validate" :declare true}]
                                       [:my-identity {:t-name "identity" :declare true}]]}
             {:export-default true
              :jsdoc-default [::mts/schema]
              :use-default-registry true
              :registry schemas})
            first)]
    (spit "out/flow-domain.core.d.ts" content)))

(comment (prep-lib))