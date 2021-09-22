(ns malli-ts.ast
  (:require [malli.core :as m]))

(defprotocol TsSchema
  (-parse-schema-node [this children options]
    "Parses a schema to TS type AST"))

(defn -ref [x] {:$ref x})

(defn -schema [schema {::keys [transform definitions] :as options}]
  (let [result (transform (m/deref schema) options)]
    (if-let [ref (m/-ref schema)]
      (do (swap! definitions assoc ref result)
          (-ref ref))
      result)))

(defmulti parse-schema-node
  (fn [name _schema _children _options] name)
  :default ::default)

(defmethod parse-schema-node ::default [_ _ _ _] {})
(defmethod parse-schema-node 'any? [_ _ _ _] {})
(defmethod parse-schema-node 'some? [_ _ _ _] {})
(defmethod parse-schema-node 'number? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'integer? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'int? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'pos-int? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'neg-int? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'nat-int? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'float? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'double? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'pos? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'neg? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'boolean? [_ _ _ _] {:type :boolean})
(defmethod parse-schema-node 'string? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'ident? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'simple-ident? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'qualified-ident? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'keyword? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'simple-keyword? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'qualified-keyword? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'symbol? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'simple-symbol? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'qualified-symbol? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'uuid? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'uri? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'decimal? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'inst? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'seqable? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'indexed? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'map? [_ _ _ _] {:type :object})
(defmethod parse-schema-node 'vector? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'list? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'seq? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'char? [_ _ _ _] {:type :string})
(defmethod parse-schema-node 'set? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'nil? [_ _ _ _] {:type :undefined})
(defmethod parse-schema-node 'false? [_ _ _ _] {:type :boolean})
(defmethod parse-schema-node 'true? [_ _ _ _] {:type :boolean})
(defmethod parse-schema-node 'zero? [_ _ _ _] {:type :number})
(defmethod parse-schema-node 'coll? [_ _ _ _] {:type :object})
(defmethod parse-schema-node 'empty? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'associative? [_ _ _ _] {:type :object})
(defmethod parse-schema-node 'sequential? [_ _ _ _] {:type :array})
(defmethod parse-schema-node 'bytes? [_ _ _ _] {:type :string})
(defmethod parse-schema-node :> [_ _ _ _] {:type :number})
(defmethod parse-schema-node :>= [_ _ _ _] {:type :number})
(defmethod parse-schema-node :< [_ _ _ _] {:type :number})
(defmethod parse-schema-node :<= [_ _ _ _] {:type :number})
(defmethod parse-schema-node := [_ _ [value] _] {:const value})
(defmethod parse-schema-node :not= [_ _ _ _] {})
(defmethod parse-schema-node :not [_ _ _ _] {})

(defmethod parse-schema-node :and [_ _ children _]
  (let [non-empty-children (filter (comp not empty?) children)]
    (if-not (empty? non-empty-children)
      {:intersection children} {})))

(defmethod parse-schema-node :or [_ _ children _] {:union children})
(defmethod parse-schema-node :orn [_ _ children _] {:union (map last children)})

(defmethod parse-schema-node ::m/val [_ _ children _] (first children))

(defmethod parse-schema-node :map [_ _ children _]
  (let [optional (->> children (filter (m/-comp :optional second)) (mapv first))
        object {:type :object
                :properties (apply array-map (mapcat (fn [[k _ s]] [k s]) children))}]
    (if (empty? optional)
      object
      (assoc object :optional optional))))

(defmethod parse-schema-node :multi [_ _ children _] {:union (mapv last children)})

(defmethod parse-schema-node :map-of [_ _ children _]
  {:type :object,  :index-signature children})

(defmethod parse-schema-node :vector [_ _ children _] {:type :array, :items (first children)})
(defmethod parse-schema-node :sequential [_ _ children _] {:type :array, :items (first children)})
(defmethod parse-schema-node :set [_ _ children _] {:type :array, :items (first children)})
(defmethod parse-schema-node :enum [_ _ children _] {:union (map #(array-map :const %) children)})
(defmethod parse-schema-node :maybe [_ _ children _] {:union (conj children {:type :undefined})})
(defmethod parse-schema-node :tuple [_ _ children _] {:type :tuple, :items children})
(defmethod parse-schema-node :re [_ schema _ options] {:type :string})
(defmethod parse-schema-node :fn [_ _ _ _] {})
(defmethod parse-schema-node :any [_ _ _ _] {})
(defmethod parse-schema-node :nil [_ _ _ _] {:type :undefined})
(defmethod parse-schema-node :string [_ schema _ _] {:type :string})
(defmethod parse-schema-node :number [_ schema _ _] {:type :number})
(defmethod parse-schema-node :int [_ schema _ _] {:type :number})
(defmethod parse-schema-node :double [_ schema _ _] {:type :number})
(defmethod parse-schema-node :boolean [_ _ _ _] {:type :boolean})
(defmethod parse-schema-node :keyword [_ _ _ _] {:type :string})
(defmethod parse-schema-node :qualified-keyword [_ _ _ _] {:type :string})
(defmethod parse-schema-node :symbol [_ _ _ _] {:type :string})
(defmethod parse-schema-node :qualified-symbol [_ _ _ _] {:type :string})
(defmethod parse-schema-node :uuid [_ _ _ _] {:type :string})

(defmethod parse-schema-node :cat [_ _ children _]
  {:type :cat :items children})

(defmethod parse-schema-node :=> [_ _ [args ret] _]
  {:type :=> :args (get args :items) :ret ret})

(defmethod parse-schema-node :function [_ _ children _]
  {:type :function :items children})

(defmethod parse-schema-node :ref [_ schema _ _] (-ref (m/-ref schema)))
(defmethod parse-schema-node :schema [_ schema _ options] (-schema schema options))
(defmethod parse-schema-node ::m/schema [_ schema _ options] (-schema schema options))

(defmethod parse-schema-node :merge [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod parse-schema-node :union [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod parse-schema-node :select-keys [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defn- -ts-schema-walker [schema _ children options]
  (if (satisfies? TsSchema schema)
    (-parse-schema-node schema children options)
    (parse-schema-node (m/type schema) schema children options)))

(defn- -parse [?schema options] (m/walk ?schema -ts-schema-walker options))

(defn ->ast
  ([?schema]
   (->ast ?schema nil))
  ([?schema options]
   (let [definitions (atom {})
         options (merge options {::m/walk-entry-vals true, ::definitions definitions, ::transform -parse})]
     (cond-> (-parse ?schema options) (seq @definitions) (assoc :definitions @definitions)))))

(comment
  (-> (->ast
       [:schema
        {:registry {:flow/poop [:map [:poop [:= "yes"]]]}}
        [:map
         [:a :int]
         [:b {:optional true} float?]
         [:c :string]
         [:d :boolean]
         [:e :flow/poop]
         [:f [:=> [:cat :int :string] :string]]
         [:g [:function
              [:=> [:cat :int] :int]
              [:=> [:cat :int :string] :string]]]]])
      prn))

