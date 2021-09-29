(ns malli-ts.tests.core
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [malli-ts.ast :refer [->ast]]
            [malli-ts.core :refer [-parse-ast-node parse-files]]
            [malli.core :as m]))

(deftest parse-functions
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

(testing "ast parsing"
  (parse-functions))
