(ns malli-ts.core-test
  (:require
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])
   [malli-ts.ast :refer [->ast]]
   [malli-ts.core :refer [->type-declaration-str -jsdoc-literal parse-ast-node
                          parse-files parse-ns-schemas] :as mts]
   [malli.instrument :as mi]
   [malli.dev.pretty :as malli-dev-pretty]))

(comment
  (mi/instrument!
   {:filters [(mi/-filter-ns 'malli-ts.core)]
    :report (malli-dev-pretty/thrower)})

  (mi/unstrument!))


(deftest function-types
  (is (= "(a:string, b:number) => {\"a\":number,\"b\":string}"
         (parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]]))))
  (is (= "(name:string, age:number) => {\"a\":number,\"b\":string}"
         (parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]])
          {:args-names ["name" "age"]})))
  (is (= "(name:string, age:number, accountNumber:string) => {\"a\":number,\"b\":string}"
         (parse-ast-node
          (->ast [:=> [:catn [:name :string] [:age :int] [:account-number string?]]
                  [:map [:a :int] [:b :string]]]))))
  (is (= "() => any"
         (parse-ast-node
          (->ast [:=> [:cat] any?])))))

(deftest declaration
  (let [schema [:map-of :string :any]
        literal (parse-ast-node (->ast schema))
        jsdoc-literal (-jsdoc-literal [["schema" (str schema)]
                                       ["author" "Mr. Poopybutthole"]])
        declaration-str (->type-declaration-str "config" literal jsdoc-literal
                                                {:export true :declare true})]
    (is (= "/**\n * @schema [:map-of :string :any]\n * @author Mr. Poopybutthole\n */\nexport var config: {[k:string]:any};"
           declaration-str))))

(def parse-files-data
  {"malli_ts.ts.d.ts" [[::k {:declare true}]
                       [::sym {:declare true}]
                       [::toClj {:declare true}]
                       [::validate {:declare true}]
                       [::answerUltimateQuestion {:declare true}]
                       [::now {:t-name "now" :declare true}]
                       [::toSha256 {:t-name "toSha256" :declare true}]]
   "random/dir/universe.d.ts" [[:random.dir.universe/answer-to-everything {:t-name "answerToEverything"}]
                               :random.dir.universe/import-test-fn]
   "random/util/string" [:random.util.string/get-default-string-decoder]})

(def parse-ns-schemas-data
  '#{malli-ts.core-test random.dir.universe random.util.string})

(def parse-files-options
  {:export-default true
   :jsdoc-default [::mts/schema]
   :use-default-schemas true
   :files-import-alias {'random.util.string "utilString"
                        "random/util/string" "utilString"}
   :registry
   {::k [:=> [:catn [:s string?]] any?]
    ::sym [:=> [:catn [:s string?]] any?]
    ::toClj [:=> [:catn [:o any?]] any?]
    ::validate [:=> [:catn [:schema any?] [:val any?]] any?]

    :random.dir.universe/answer-to-everything [:enum 42]
    :random.dir.universe/import-test-fn :random.util.string/get-default-string-decoder

    :random.util.string/get-default-string-decoder
    [:=> :catn (mts/external-type "StringDecoder" :import-path "string_decoder" :schema some?)]
    ::answerUltimateQuestion [:=> :cat :random.dir.universe/answer-to-everything]

    :global/date (mts/external-type "Date" :schema inst?)
    ::now [:=> [:cat] :global/date]

    ;; explicit "absolute" path (already default )
    :crypto/hash (mts/external-type "Hash" :import-path {:absolute "crypto"} :import-alias "crypto")
    ::toSha256 [:=> [:catn [:s string?]] :crypto/hash]}})

(comment
  (parse-files parse-files-data parse-files-options)
  (parse-ns-schemas parse-ns-schemas-data parse-files-options))

(deftest parsing
  (is (= {"user.d.ts" "type event = {\"value\":any,\"timestamp\":Date};"}
         (parse-ns-schemas
          #{'user}
          {:registry {:user/event [:map
                                   [:value any?]
                                   [:timestamp inst?]]}})
         (parse-ns-schemas
          #{'user}
          {:registry {:date [inst? {::mts/external-type "Date"}]
                      :user/event [:map
                                   [:value any?]
                                   [:timestamp :date]]}})))

  (is (= {"malli_ts.ts.d.ts"
          "import * as crypto from 'crypto';\nimport * as random_dir_universe from './random/dir/universe.d.ts';\n\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport var k: (s:string) => any;\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport var sym: (s:string) => any;\n/**\n * @schema [:=> [:catn [:o any?]] any?]\n */\nexport var to_clj: (o:any) => any;\n/**\n * @schema [:=> [:catn [:schema any?] [:val any?]] any?]\n */\nexport var validate: (schema:any, val:any) => any;\n/**\n * @schema [:=> :cat :random.dir.universe/answer-to-everything]\n */\nexport var answer_ultimate_question: () => random_dir_universe.answerToEverything;\n/**\n * @schema [:=> :cat :global/date]\n */\nexport var now: () => Date;\n/**\n * @schema [:=> [:catn [:s string?]] :crypto/hash]\n */\nexport var toSha256: (s:string) => crypto.Hash;",
          "random/dir/universe.d.ts"
          "import * as utilString from './../util/string';\n\n/**\n * @schema [:enum 42]\n */\nexport type answerToEverything = (42);\n/**\n * @schema :random.util.string/get-default-string-decoder\n */\nexport type import_test_fn = utilString.get_default_string_decoder;",
          "random/util/string.d.ts"
          "import * as string_decoder from 'string_decoder';\n\n/**\n * @schema [:=> :catn [some? #:malli-ts.core{:external-type \"StringDecoder\", :t-path {:absolute \"string_decoder\"}}]]\n */\nexport type get_default_string_decoder = () => string_decoder.StringDecoder;"}
         (parse-files parse-files-data parse-files-options)))

  (is (= {"random.dir.universe.d.ts"
          "import * as utilString from './random.util.string';\n\n/**\n * @schema [:enum 42]\n */\nexport type answer_to_everything = (42);\n/**\n * @schema :random.util.string/get-default-string-decoder\n */\nexport type import_test_fn = utilString.get_default_string_decoder;",
          "random.util.string.d.ts"
          "import * as string_decoder from 'string_decoder';\n\n/**\n * @schema [:=> :catn [some? #:malli-ts.core{:external-type \"StringDecoder\", :t-path {:absolute \"string_decoder\"}}]]\n */\nexport type get_default_string_decoder = () => string_decoder.StringDecoder;",
          "malli_ts.core_test.d.ts"
          "import * as crypto from 'crypto';\nimport * as random_dir_universe from './random.dir.universe';\n\n/**\n * @schema [:=> :cat :random.dir.universe/answer-to-everything]\n */\nexport type answer_ultimate_question = () => random_dir_universe.answer_to_everything;\n/**\n * @schema [:=> [:catn [:schema any?] [:val any?]] any?]\n */\nexport type validate = (schema:any, val:any) => any;\n/**\n * @schema [:=> :cat :global/date]\n */\nexport type now = () => Date;\n/**\n * @schema [:=> [:catn [:o any?]] any?]\n */\nexport type to_clj = (o:any) => any;\n/**\n * @schema [:=> [:catn [:s string?]] :crypto/hash]\n */\nexport type to_sha_256 = (s:string) => crypto.Hash;\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport type sym = (s:string) => any;\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport type k = (s:string) => any;"}
         (parse-ns-schemas parse-ns-schemas-data parse-files-options))))

(comment
  (testing "ast parsing"
    (function-types))
  (testing "declaration"
    (declaration))
  (testing "parsing"
    (parsing)))

