(ns malli-ts.core-test
  (:require
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])
   [malli-ts.ast :refer [->ast]]
   [malli-ts.core :refer [->type-declaration-str -jsdoc-literal parse-ast-node
                          parse-files] :as mts]
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
  {"malli-ts.ts.d.ts" [[:k {:declare true}]
                       [:sym {:declare true}]
                       [:toClj {:declare true}]
                       [:validate {:declare true}]
                       [:answerUltimateQuestion {:declare true}]
                       [:now {:t-name "now" :declare true}]
                       [:toSha256 {:t-name "toSha256" :declare true}]]
   "random/dir/universe.d.ts" [[:random.dir.universe/answer-to-everything {:t-name "answerToEverything"}]]})

(def parse-files-options
  {:export-default true
   :jsdoc-default [::mts/schema]
   :use-default-schemas true
   :registry
   {:k [:=> [:catn [:s string?]] any?]
    :sym [:=> [:catn [:s string?]] any?]
    :toClj [:=> [:catn [:o any?]] any?]
    :validate [:=> [:catn [:schema any?] [:val any?]] any?]
    
    :random.dir.universe/answer-to-everything [:enum 42]
    :answerUltimateQuestion [:=> :cat :random.dir.universe/answer-to-everything]

    :global/date (mts/external-type "Date")
    :now [:=> [:cat] :global/date]

    :crypto/hash (mts/external-type "Hash" {:absolute "crypto"} "crypto")
    :toSha256 [:=> [:catn [:s string?]] :crypto/hash]}})

(comment
  (prn (parse-files parse-files-data parse-files-options)))

(deftest parsing
  (is (= {"malli-ts.ts.d.ts" "import * as crypto from 'crypto';\nimport * as randomDir from '../random/dir/universe.d.ts';\n\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport var k: (s:string) => any;\n/**\n * @schema [:=> [:catn [:s string?]] any?]\n */\nexport var sym: (s:string) => any;\n/**\n * @schema [:=> [:catn [:o any?]] any?]\n */\nexport var toClj: (o:any) => any;\n/**\n * @schema [:=> [:catn [:schema any?] [:val any?]] any?]\n */\nexport var validate: (schema:any, val:any) => any;\n/**\n * @schema [:=> :cat :random.dir.universe/answer-to-everything]\n */\nexport var answerUltimateQuestion: () => randomDir.answerToEverything;\n/**\n * @schema [:=> :cat :global/date]\n */\nexport var now: () => Date;\n/**\n * @schema [:=> [:catn [:s string?]] :crypto/hash]\n */\nexport var toSha256: (s:string) => crypto.Hash;", "random/dir/universe.d.ts" "/**\n * @schema [:enum 42]\n */\nexport type answerToEverything = (42);"}
         (parse-files parse-files-data parse-files-options))))

(comment
  (testing "ast parsing"
    (function-types))
  (testing "declaration"
    (declaration)))

