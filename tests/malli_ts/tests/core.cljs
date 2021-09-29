(ns malli-ts.tests.core
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [malli-ts.ast :refer [->ast]]
            [malli-ts.core :refer [-parse-ast-node parse-files ->type-declaration-str -jsdoc-literal]]
            [malli.core :as m]))

(deftest function-types
  (is (= (-parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]]))
         "(a:string, b:number) => {\"a\":number,\"b\":string}"))
  (is (= (-parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]])
          {:args-names ["name" "age"]})
         "(name:string, age:number) => {\"a\":number,\"b\":string}"))
  (is (= (-parse-ast-node
          (->ast [:=> [:catn [:name :string] [:age :int] [:account-number string?]]
                  [:map [:a :int] [:b :string]]]))
         "(name:string, age:number, accountNumber:string) => {\"a\":number,\"b\":string}"))
  (is (= (-parse-ast-node
          (->ast [:=> [:cat] any?]))
         "() => any")))

(deftest declaration
  (let [schema [:map-of :string :any]
        literal (-parse-ast-node (->ast schema))
        jsdoc-literal (-jsdoc-literal [["schema" (str schema)]
                                       ["author" "Mr. Poopybutthole"]])
        declaration-str (->type-declaration-str "config" literal jsdoc-literal
                                                {:export true :declare true})]
    (is (= declaration-str
           "/**\n * @schema [:map-of :string :any]\n * @author Mr. Poopybutthole\n */\nexport var config: {[k:string]:any};"))))

(testing "ast parsing"
  (function-types))
