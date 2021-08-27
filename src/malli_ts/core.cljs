(ns malli-ts.core
  (:require [malli-ts.parse :refer [parse-ast]]
            [malli.core :as m]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as string]
            ["path" :as path]))

(defn- -dispatch-parse-ast-node
  [node options]
  (cond
    (not (some? node)) :nil-node
    (:$ref node) :$ref
    (:type node) [:type (:type node)]
    (:union node) :union
    (:intersection node) :intersection
    (some? (:const node)) :const
    :else [:type :any]))

(defmulti ^:private -parse-ast-node
  #'-dispatch-parse-ast-node)

(defmethod -parse-ast-node :$ref
  [{:keys [$ref] :as node} {:keys [deref-types
                                   schema-id->type-desc
                                   files-import-alias
                                   file-imports*
                                   file]
                            :as options}]
  (if (get deref-types $ref)
    (-parse-ast-node (get-in node [:definitions $ref]) options)
    (let [ref-file (get-in schema-id->type-desc [$ref :file])
          import-alias (get files-import-alias ref-file)
          ref-type-name (get-in schema-id->type-desc [$ref :t-name])
          same-file? (= file ref-file)]
      (when-not same-file? 
        (swap! file-imports* update file clojure.set/union #{ref-file}))
      (str (if-not same-file? (str import-alias ".")) ref-type-name))))

(comment
  (-parse-ast-node
   {:$ref :flow/person
    (:definitions) {:flow/person (transform [:tuple :string :int])}}
   {:deref-types {:flow/person false}
    :schema-id->type-desc
    {:flow/person {:t-name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias {"flow/person/index.d.ts" "fp"}})
  
  (-parse-ast-node
   {:$ref :flow/person
    :definitions {:flow/person (transform [:tuple :string :int])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc
    {:flow/person {:t-name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias {"flow/person/index.d.ts" "fp"}}))

(defmethod -parse-ast-node [:type :number] [_ _] "number")
(defmethod -parse-ast-node [:type :string] [_ _] "string")
(defmethod -parse-ast-node [:type :boolean] [_ _] "boolean")
(defmethod -parse-ast-node [:type :any] [_ _] "any")
(defmethod -parse-ast-node [:type :undefined] [_ _] "undefined")
(defmethod -parse-ast-node :const [{:keys [const] :as node} options]  
  (cond
    (keyword? const) (str \" (name const) \")
    (string? const) (str \" const \")
    (some #(% const) [boolean? number?]) (str const)
    :else (-parse-ast-node {:type :any} options)))

(defmethod -parse-ast-node [:type :array] [{:keys [items]} options]
  (str "Array<" (-parse-ast-node items options) ">"))

(comment
  (-parse-ast-node {:type :array :items {:type :number}} nil))

(defmethod -parse-ast-node [:type :tuple] [{:keys [items]} options]
  (str "[" (string/join "," (map #(-parse-ast-node % options) items)) "]"))

(comment (-parse-ast-node (transform [:tuple :int :string :boolean])))

(defmethod -parse-ast-node :union [{items :union} options]
  (str "(" (string/join "|" (map #(-parse-ast-node % options) items)) ")"))

(defmethod -parse-ast-node :intersection [{items :intersection} options]
  (str "(" (string/join "&" (map #(-parse-ast-node % options) items)) ")"))

(comment
  (-parse-ast-node (transform [:enum 1 :something false]))
  (-parse-ast-node (transform [:maybe :string]))
  (-parse-ast-node (transform [:or :string :int]))
  (-parse-ast-node (transform [:orn [:t-name :string] [:age :int]]))
  ;; I know this doesn't make sense
  (-parse-ast-node (transform [:and :string :boolean])))

(defmethod -parse-ast-node [:type :object] [{:keys [properties
                                                    optional
                                                    index-signature]}
                                            {:keys [default-to-camel-case]
                                             :as options}]
  (let [idx-sign-literal (if index-signature
                           (str "[k:" (-parse-ast-node (first index-signature) options) "]:"
                                (-parse-ast-node (second index-signature) options)))
        properties-literal (if-not (empty? properties)
                             (string/join
                              ","
                              (map (fn [[k v]]
                                     (let [property-name (if default-to-camel-case
                                                           (csk/->camelCase (name k))
                                                           (name k))]
                                       (str \" property-name \"
                                            (if (get optional k) "?") ":"
                                            (-parse-ast-node v options))))
                                   properties)))]
    (str "{" (string/join "," (filter (comp not string/blank?)
                                      [idx-sign-literal properties-literal]))
         "}")))

(comment
  (-parse-ast-node (transform [:map [:a :int] [:b :string]]))
  (-parse-ast-node (transform [:map-of :string :int]))
  (-parse-ast-node
   {:$ref :flow/person
    :definitions {:flow/person (transform [:map [:t-name string?] [:age pos-int?]])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc {:flow/person {:t-name "FlowPerson"
                                         :file "flow/person/index.d.ts"}}
    :files-import-alias {"flow/person/index.d.ts" "fp"}}))

(defn import-literal
  [from alias]
  (str "import * as " alias " from " \' from \' \;))

(comment
  (import-literal
   (path/relative (path/dirname "flow/person/index.d.ts") "flow/index.d.ts")
   "flow"))

(defn ->type-declaration-str
  [type-name literal options]
  (let [{:keys [export]} options]
    (str (if export "export " nil)
         "type "  type-name " = " literal ";")))

(defn parse-files
  [file->schema-type-vs options]
  (let [schema-id->type-desc
        (reduce
         (fn [m [file schema-type-vs]]
           (merge m (reduce
                     (fn [m [schema-id type-desc]]
                       (assoc m schema-id (assoc type-desc :file file)))
                     {} schema-type-vs)))
         {} file->schema-type-vs)

        options (merge options {:schema-id->type-desc schema-id->type-desc
                                :file-imports* (atom {})})

        schema-id->type-desc ;; assocs literal into type-desc
        (reduce
         (fn [m [file schema-type-vs]]
           (reduce
            (fn [m [schema-id {:keys [t-name]}]]
              (assoc-in
               m [schema-id :literal] 
               (-parse-ast-node (parse-ast schema-id options)
                                (merge options
                                       {:deref-types {schema-id true}
                                        :file file}))))
            m schema-type-vs))
         schema-id->type-desc file->schema-type-vs)

        {:keys [export-default files-import-alias file-imports*]} options        

        files (map (fn [[k _]] k) file->schema-type-vs)

        file->import-literals
        (reduce
         (fn [m file]
           (assoc-in
            m [file]
            (map
             (fn [import-file]
               (import-literal
                (path/relative (path/dirname file) import-file)
                (get files-import-alias import-file)))
             (get @file-imports* file))))
         {} files)

        file->type-literals
        (reduce
         (fn [m [file scheva-type-vs]]
           (assoc
            m file
            (map
             (fn [[schema-id _]]
               (let [{:keys [t-name literal export]} (get schema-id->type-desc schema-id)]
                 (->type-declaration-str
                  t-name literal
                  {:export (if (some? export) export export-default)})))
             scheva-type-vs)))
         {} file->schema-type-vs)

        file-contents
        (reduce (fn [m
                    file]
                  (assoc-in
                   m [file]
                   (let [import (string/join "\n" (get file->import-literals file))
                         types (string/join "\n" (get file->type-literals file))]
                     (str (if-not (string/blank? import) (str import "\n\n"))
                          types))))
                {} files)]
    file-contents))

