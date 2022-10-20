(ns malli-ts.core-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [malli-ts.ast :refer [->ast]]
            [malli-ts.core :refer [parse-ast-node ->type-declaration-str -jsdoc-literal]]))

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

(comment
  (testing "ast parsing"
    (function-types))
  (testing "declaration"
    (declaration)))

