(ns malli-ts.tests.core
  (:require [cljs.test :refer-macros [deftest is run-tests testing]]
            [malli-ts.ast :refer [->ast]]
            [malli-ts.core :refer [-parse-ast-node parse-files]]
            [malli.core :as m]))

(deftest parse-functions
  (is (= (-parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]]))
         "function (a:string, b:number): {\"a\":number,\"b\":string}"))
  (is (= (-parse-ast-node
          (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]])
          {:args-names ["name" "age"]})
         "function (name:string, age:number): {\"a\":number,\"b\":string}")))


(testing "ast parsing"
  (parse-functions))
