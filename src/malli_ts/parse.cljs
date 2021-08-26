(ns malli-ts.parse
  (:require [malli-ts.transform :refer [transform]]
            [malli.core :as m]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as string]
            ["path" :as path]))

(comment
  (ns-unmap 'malli-ts.export.parse '-parse-node))

(defn- -dispatch-parse-node
  [node options]
  (cond
    (:$ref node) :$ref
    (:type node) [:type (:type node)]
    (:union node) :union
    (:intersection node) :intersection
    (some? (:const node)) :const
    :else [:type :any]))

(defmulti ^:private -parse-node
  #'-dispatch-parse-node)

(defmethod -parse-node :$ref
  [{:keys [$ref] :as node} {:keys [deref-types
                                   schema-id->type-desc
                                   files-import-alias
                                   file-imports*
                                   file]
                            :as options}]
  (if (get deref-types $ref)
    (-parse-node (get-in node [:definitions $ref]) options)
    (let [ref-file (get-in schema-id->type-desc [$ref :file])
          import-alias (get files-import-alias ref-file)
          ref-type-name (get-in schema-id->type-desc [$ref :name])
          same-file? (= file ref-file)]
      (when-not same-file? 
        (swap! file-imports* update file clojure.set/union #{ref-file}))
      (str (if-not same-file? (str import-alias ".")) ref-type-name))))

(comment
  (-parse-node
   {:$ref :flow/person
    (:definitions) {:flow/person (transform [:tuple :string :int])}}
   {:deref-types {:flow/person false}
    :schema-id->type-desc
    {:flow/person {:name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias {"flow/person/index.d.ts" "fp"}})
  
  (-parse-node
   {:$ref :flow/person
    :definitions {:flow/person (transform [:tuple :string :int])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc
    {:flow/person {:name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias {"flow/person/index.d.ts" "fp"}}))

(defmethod -parse-node [:type :number] [_ _] "number")
(defmethod -parse-node [:type :string] [_ _] "string")
(defmethod -parse-node [:type :boolean] [_ _] "boolean")
(defmethod -parse-node [:type :any] [_ _] "any")
(defmethod -parse-node [:type :undefined] [_ _] "undefined")
(defmethod -parse-node :const [{:keys [const] :as node} options]  
  (cond
    (keyword? const) (str \" (name const) \")
    (string? const) (str \" const \")
    (some #(% const) [boolean? number?]) (str const)
    :else (-parse-node {:type :any} options)))

(defmethod -parse-node [:type :array] [{:keys [items]} options]
  (str "Array<" (-parse-node items options) ">"))

(comment
  (-parse-node {:type :array :items {:type :number}} nil))

(defmethod -parse-node [:type :tuple] [{:keys [items]} options]
  (str "[" (string/join "," (map #(-parse-node % options) items)) "]"))

(comment (-parse-node (transform [:tuple :int :string :boolean])))

(defmethod -parse-node :union [{items :union} options]
  (str "(" (string/join "|" (map #(-parse-node % options) items)) ")"))

(defmethod -parse-node :intersection [{items :intersection} options]
  (str "(" (string/join "&" (map #(-parse-node % options) items)) ")"))

(comment
  (-parse-node (transform [:enum 1 :something false]))
  (-parse-node (transform [:maybe :string]))
  (-parse-node (transform [:or :string :int]))
  (-parse-node (transform [:orn [:name :string] [:age :int]]))
  ;; I know this doesn't make sense
  (-parse-node (transform [:and :string :boolean])))

(defmethod -parse-node [:type :object] [{:keys [properties
                                                optional
                                                index-signature]}
                                        {:keys [default-to-camel-case]
                                         :as options}]
  (let [idx-sign-literal (if index-signature
                           (str "[k:" (-parse-node (first index-signature) options) "]:"
                                (-parse-node (second index-signature) options)))
        properties-literal (if-not (empty? properties)
                             (string/join
                              ","
                              (map (fn [[k v]]
                                     (let [property-name (if default-to-camel-case
                                                           (csk/->camelCase (name k))
                                                           (name k))]
                                       (str \" property-name \"
                                            (if (get optional k) "?") ":"
                                            (-parse-node v options))))
                                   properties)))]
    (str "{" (string/join "," (filter (comp not string/blank?)
                                      [idx-sign-literal properties-literal]))
         "}")))

(comment
  (-parse-node (transform [:map [:a :int] [:b :string]]))
  (-parse-node (transform [:map-of :string :int]))
  (-parse-node
   {:$ref :flow/person
    :definitions {:flow/person (transform [:map [:name string?] [:age pos-int?]])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc {:flow/person {:name "FlowPerson"
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

(defn parse
  [schemas-v options]
  (let [schema-id->type-desc (into {} schemas-v)
        options (merge options {:schema-id->type-desc schema-id->type-desc
                                :file-imports* (atom {})})
        schema-id->type-desc
        (reduce (fn [m [schema-id {:keys [file] :as type-desc}]]
                  (assoc-in
                   m [schema-id :literal]
                   (-parse-node (transform schema-id options)
                                (merge options
                                       {:deref-types {schema-id true}
                                        :file file}))))
                schema-id->type-desc
                schemas-v)

        file->type-descs
        (reduce (fn [m [schema-id {:keys [file]}]]
                  (update-in
                   m [file]
                   conj (get schema-id->type-desc schema-id)))
                {} schemas-v)

        {:keys [export-default files-import-alias file-imports*]} options

        files (map (fn [[k _]] k) file->type-descs)

        file->import-literals
        (reduce (fn [m file]
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
        (reduce (fn [m file]
                  (assoc-in
                   m [file]
                   (map
                    (fn [{:keys [name literal export]}]
                      (->type-declaration-str
                       name literal
                       {:export (if (some? export) export export-default)}))
                    (get file->type-descs file))))
                {} files)

        file-contents
        (reduce (fn [m file]
                  (assoc-in
                   m [file]
                   (let [import (string/join "\n" (get file->import-literals file))
                         types (string/join "\n" (get file->type-literals file))]
                     (str (if-not (string/blank? import) (str import "\n\n"))
                          types))))
                {} files)]
    file-contents))

;; TODO: UPDATE file-imports*

(comment
  (parse
   [[:flow/person {:name "FlowPerson"
                   :file "flow/index.d.ts"}]
    ] 
   {:export-default true
    :files-import-alias {"flow/index.d.ts" "flow"}
    :registry {:flow/person (m/schema [:map [:name string?] [:age pos-int?]])}}))

