(ns malli-ts.transform
  (:require [malli.core :as m]))

(defprotocol TsSchema
  (-accept [this children options] "transforms schema to TS Schema"))

(defn -ref [x] {:$ref x})

(defn -schema [schema {::keys [transform definitions] :as options}]
  (let [result (transform (m/deref schema) options)]
    (if-let [ref (m/-ref schema)]
      (do (swap! definitions assoc ref result)
          (-ref ref))
      result)))

(defmulti accept (fn [name _schema _children _options] name) :default ::default)

(defmethod accept ::default [name _ _ _] {})
(defmethod accept 'any? [_ _ _ _] {})
(defmethod accept 'some? [_ _ _ _] {})
(defmethod accept 'number? [_ _ _ _] {:type :number})
(defmethod accept 'integer? [_ _ _ _] {:type :number})
(defmethod accept 'int? [_ _ _ _] {:type :number})
(defmethod accept 'pos-int? [_ _ _ _] {:type :number})
(defmethod accept 'neg-int? [_ _ _ _] {:type :number})
(defmethod accept 'nat-int? [_ _ _ _] {:type :number})
(defmethod accept 'float? [_ _ _ _] {:type :number})
(defmethod accept 'double? [_ _ _ _] {:type :number})
(defmethod accept 'pos? [_ _ _ _] {:type :number})
(defmethod accept 'neg? [_ _ _ _] {:type :number})
(defmethod accept 'boolean? [_ _ _ _] {:type :boolean})
(defmethod accept 'string? [_ _ _ _] {:type :string})
(defmethod accept 'ident? [_ _ _ _] {:type :string})
(defmethod accept 'simple-ident? [_ _ _ _] {:type :string})
(defmethod accept 'qualified-ident? [_ _ _ _] {:type :string})
(defmethod accept 'keyword? [_ _ _ _] {:type :string})
(defmethod accept 'simple-keyword? [_ _ _ _] {:type :string})
(defmethod accept 'qualified-keyword? [_ _ _ _] {:type :string})
(defmethod accept 'symbol? [_ _ _ _] {:type :string})
(defmethod accept 'simple-symbol? [_ _ _ _] {:type :string})
(defmethod accept 'qualified-symbol? [_ _ _ _] {:type :string})
(defmethod accept 'uuid? [_ _ _ _] {:type :string})
(defmethod accept 'uri? [_ _ _ _] {:type :string})
(defmethod accept 'decimal? [_ _ _ _] {:type :number})
(defmethod accept 'inst? [_ _ _ _] {:type :string})
(defmethod accept 'seqable? [_ _ _ _] {:type :array})
(defmethod accept 'indexed? [_ _ _ _] {:type :array})
(defmethod accept 'map? [_ _ _ _] {:type :object})
(defmethod accept 'vector? [_ _ _ _] {:type :array})
(defmethod accept 'list? [_ _ _ _] {:type :array})
(defmethod accept 'seq? [_ _ _ _] {:type :array})
(defmethod accept 'char? [_ _ _ _] {:type :string})
(defmethod accept 'set? [_ _ _ _] {:type :array})
(defmethod accept 'nil? [_ _ _ _] {:type :undefined})
(defmethod accept 'false? [_ _ _ _] {:type :boolean})
(defmethod accept 'true? [_ _ _ _] {:type :boolean})
(defmethod accept 'zero? [_ _ _ _] {:type :number})
(defmethod accept 'coll? [_ _ _ _] {:type :object})
(defmethod accept 'empty? [_ _ _ _] {:type :array})
(defmethod accept 'associative? [_ _ _ _] {:type :object})
(defmethod accept 'sequential? [_ _ _ _] {:type :array})
(defmethod accept 'bytes? [_ _ _ _] {:type :string})
(defmethod accept :> [_ _ _ _] {:type :number})
(defmethod accept :>= [_ _ _ _] {:type :number})
(defmethod accept :< [_ _ _ _] {:type :number})
(defmethod accept :<= [_ _ _ _] {:type :number})
(defmethod accept := [_ _ [value] _] {:const value})
(defmethod accept :not= [_ _ _ _] {})
(defmethod accept :not [_ _ _ _] {})

(defmethod accept :and [_ _ children _]
  (let [non-empty-children (filter (comp not empty?) children)]
    (if-not (empty? non-empty-children)
      {:intersection children} {})))

(defmethod accept :or [_ _ children _] {:union children})
(defmethod accept :orn [_ _ children _] {:union (map last children)})

(defmethod accept ::m/val [_ _ children _] (first children))

(defmethod accept :map [_ _ children _]
  (let [optional (->> children (filter (m/-comp :optional second)) (mapv first))
        object {:type :object
                :properties (apply array-map (mapcat (fn [[k _ s]] [k s]) children))}]
    (if (empty? optional)
      object
      (assoc object :optional optional))))

(defmethod accept :multi [_ _ children _] {:union (mapv last children)})

(defmethod accept :map-of [_ _ children _]
  {:type :object,  :index-signature children})

(defmethod accept :vector [_ _ children _] {:type :array, :items (first children)})
(defmethod accept :sequential [_ _ children _] {:type :array, :items (first children)})
(defmethod accept :set [_ _ children _] {:type :array, :items (first children)})
(defmethod accept :enum [_ _ children _] {:union (map #(array-map :const %) children)})
(defmethod accept :maybe [_ _ children _] {:union (conj children {:type :undefined})})
(defmethod accept :tuple [_ _ children _] {:type :tuple, :items children})
(defmethod accept :re [_ schema _ options] {:type :string})
(defmethod accept :fn [_ _ _ _] {})
(defmethod accept :any [_ _ _ _] {})
(defmethod accept :nil [_ _ _ _] {:type :undefined})
(defmethod accept :string [_ schema _ _] {:type :string})
(defmethod accept :number [_ schema _ _] {:type :number})
(defmethod accept :int [_ schema _ _] {:type :number})
(defmethod accept :double [_ schema _ _] {:type :number})
(defmethod accept :boolean [_ _ _ _] {:type :boolean})
(defmethod accept :keyword [_ _ _ _] {:type :string})
(defmethod accept :qualified-keyword [_ _ _ _] {:type :string})
(defmethod accept :symbol [_ _ _ _] {:type :string})
(defmethod accept :qualified-symbol [_ _ _ _] {:type :string})
(defmethod accept :uuid [_ _ _ _] {:type :string})
(defmethod accept :=> [_ _ _ _] {})
(defmethod accept :function [_ _ _ _] {})
(defmethod accept :ref [_ schema _ _] (-ref (m/-ref schema)))
(defmethod accept :schema [_ schema _ options] (-schema schema options))
(defmethod accept ::m/schema [_ schema _ options] (-schema schema options))

(defmethod accept :merge [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod accept :union [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod accept :select-keys [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defn- -ts-schema-walker [schema _ children options]
  (if (satisfies? TsSchema schema)
    (-accept schema children options)
    (accept (m/type schema) schema children options)))

(defn -transform [?schema options] (m/walk ?schema -ts-schema-walker options))

;;
;; public api
;;

(defn transform
  ([?schema]
   (transform ?schema nil))
  ([?schema options]
   (let [definitions (atom {})
         options (merge options {::m/walk-entry-vals true, ::definitions definitions, ::transform -transform})]
     (cond-> (-transform ?schema options) (seq @definitions) (assoc :definitions @definitions)))))

(comment
  (transform
   [:schema
    {:registry {:flow/poop [:map [:poop [:= "yes"]]]}}
    [:map
     [:a :int]
     [:b {:optional true} float?]
     [:c :string]
     [:d :boolean]
     [:e :flow/poop]]]))

(comment
  (transform :flow/poop {:registry {:flow/poop (m/schema any?)}}))
