(ns malli-ts.core
  (:require [malli-ts.ast :refer [->ast]]
            [malli.core :as m]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as string]
            [clojure.set :as set]
            ["path" :as path]))

(defn- -dispatch-parse-ast-node
  [node options]
  (cond
    (-> node :schema (m/properties options) :external-type) :external-type
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
                                   files-import-alias*
                                   file-imports*
                                   file]
                            :as options}]
  (if (or (get deref-types $ref) (not (get schema-id->type-desc $ref)))
    (-parse-ast-node
     (or (get-in node [:definitions $ref])
         (->ast (:schema node)))
     options)
    (let [ref-file (get-in schema-id->type-desc [$ref :file])
          import-alias (get @files-import-alias* ref-file)
          ref-type-name (get-in schema-id->type-desc [$ref :t-name])
          same-file? (= file ref-file)]
      (when-not same-file? 
        (swap! file-imports* update file set/union #{ref-file}))
      (str (if-not same-file? (str import-alias ".")) ref-type-name))))

(comment
  (-parse-ast-node
   {:$ref :flow/person
    (:definitions) {:flow/person (parse-ast [:tuple :string :int])}}
   {:deref-types {:flow/person false}
    :schema-id->type-desc
    {:flow/person {:t-name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias* (atom {"flow/person/index.d.ts" "fp"})})
  
  (-parse-ast-node
   {:$ref :flow/person
    :definitions {:flow/person (parse-ast [:tuple :string :int])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc
    {:flow/person {:t-name "FlowPerson"
                   :file "flow/person/index.d.ts"}}
    :files-import-alias* (atom {"flow/person/index.d.ts" "fp"})}))

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

(comment (-parse-ast-node (parse-ast [:tuple :int :string :boolean])))

(defmethod -parse-ast-node :union [{items :union} options]
  (str "(" (string/join "|" (map #(-parse-ast-node % options) items)) ")"))

(defmethod -parse-ast-node :intersection [{items :intersection} options]
  (str "(" (string/join "&" (map #(-parse-ast-node % options) items)) ")"))

(comment
  (-parse-ast-node (parse-ast [:enum 1 :something false]))
  (-parse-ast-node (parse-ast [:maybe :string]))
  (-parse-ast-node (parse-ast [:or :string :int]))
  (-parse-ast-node (parse-ast [:orn [:t-name :string] [:age :int]]))
  ;; I know this doesn't make sense
  (-parse-ast-node (parse-ast [:and :string :boolean])))

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
                                            (do
                                              (-parse-ast-node v options)))))
                                   properties)))]
    (str "{" (string/join "," (filter (comp not string/blank?)
                                      [idx-sign-literal properties-literal]))
         "}")))

(defmethod -parse-ast-node :external-type [{:keys [schema]}
                                           {:keys [file
                                                   files-import-alias*
                                                   file-imports*]
                                            :as options}]
  (let [{:keys [t-name t-path t-alias]} (:external-type (m/properties schema))
        is-imported-already (@file-imports* t-path)
        canonical-alias (get @files-import-alias* t-path)
        import-alias (if canonical-alias
                       canonical-alias
                       (if t-alias
                         t-alias
                         (csk/->camelCase (string/join "-" (take-last 2 (string/split t-path "/"))))))]
    (when-not is-imported-already
      (swap! file-imports* update file set/union #{t-path}))
    (when-not canonical-alias
      (swap! files-import-alias* assoc t-path import-alias))
    (str import-alias "." t-name)))

(comment
  (-parse-ast-node (parse-ast [:map [:a :int] [:b :string]]))
  (-parse-ast-node (parse-ast [:map-of :string :int]))
  (-parse-ast-node
   {:$ref :flow/person
    :definitions {:flow/person (parse-ast [:map [:t-name string?] [:age pos-int?]])}}
   {:deref-types {:flow/person true}
    :schema-id->type-desc {:flow/person {:t-name "FlowPerson"
                                         :file "flow/person/index.d.ts"}}
    :files-import-alias* (atom {"flow/person/index.d.ts" "fp"})}))

(defn- letter-args
  ([letter-arg]
   (if letter-arg
     (let [letter-count (.substring letter-arg 1)
           next-count (if-not (empty? letter-count)
                        (-> letter-count js/Number inc)
                        1)
           char-code (.charCodeAt letter-arg 0)      
           z? (= char-code 122)
           next-letter (if-not z?
                         (.fromCharCode js/String (inc char-code))
                         "a")
           next-letter-arg (str next-letter (if z? next-count letter-count))]
       (cons next-letter-arg
             (lazy-seq (letter-args next-letter-arg))))
     (cons "a" (lazy-seq (letter-args "a")))))
  ([] (letter-args nil)))

(comment (take 5 (letter-args)))

(defmethod -parse-ast-node [:type :=>] [{:keys [args ret]}
                                        {:keys [args-names]
                                         :as options}]
  (let [args-type (get args :type)
        args-items (get args :items)
        args-names (cond
                     args-names args-names
                     (= args-type :catn) (map (fn [[n]] (csk/->camelCaseString n))
                                              args-items)
                     :else (take (count args) (letter-args)))
        args (if (= args-type :catn)
               (map (fn [[_ a]] a) args-items)
               args-items)]
    (str "("
         (string/join ", " (map (fn [arg-name arg]
                                  (str arg-name ":" (-parse-ast-node arg options)))
                                args-names args))
         ") => " (-parse-ast-node ret options))))

(comment
  (-parse-ast-node
   (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]])))

(defn import-literal
  [from alias]
  (str "import * as " alias " from " \' from \' \;))

(comment
  (import-literal
   (path/relative (path/dirname "flow/person/index.d.ts") "flow/index.d.ts")
   "flow"))

(defn ->type-declaration-str
  [type-name literal jsdoc-literal options]
  (let [{:keys [export declare]} options]
    (str (if jsdoc-literal (str jsdoc-literal \newline))
         (if export "export ")
         (if declare "var " "type ")
         type-name (if declare ": " " = ") literal ";")))

(defn -dispatch-provide-jsdoc [jsdoc-k _ _] jsdoc-k)

(defmulti provide-jsdoc #'-dispatch-provide-jsdoc)

(defmethod provide-jsdoc ::schema
  [jsdoc-k schema-id t-options options]
  ["schema" (-> schema-id (m/deref options) m/form str)])

(defn -jsdoc-literal
  [jsdoc-pairs]
  (if-not (empty? jsdoc-pairs)
    (str "/**\n"
         (->> jsdoc-pairs
              (map (fn [[attribute value]] (str " * @" attribute " " value)))
              (string/join "\n"))
         "\n */")))

(comment
  (println (-jsdoc-literal [["schema" (str '[:map-of any?])]
                            ["author" "Mr. Poopybutthole"]])))

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
                                :file-imports* (atom {})
                                :files-import-alias* (atom {})})

        jsdoc-default (get options :jsdoc-default)

        schema-id->type-desc ;; assocs literal into type-desc
        (reduce
         (fn [m [file schema-type-vs]]
           (reduce
            (fn [m [schema-id {:keys [t-name jsdoc] :as t-options}]]
              (let [literal (-parse-ast-node (->ast schema-id options)
                                             (merge options
                                                    {:deref-types {schema-id true}
                                                     :file file
                                                     :t-options t-options}))
                    jsdoc-literal
                    (->> (concat jsdoc-default jsdoc)
                         (map #(provide-jsdoc % schema-id t-options options))
                         -jsdoc-literal)]
                (-> m
                    (assoc-in [schema-id :literal] literal)
                    (assoc-in [schema-id :jsdoc-literal] jsdoc-literal))))
            m schema-type-vs))
         schema-id->type-desc file->schema-type-vs)

        {:keys [export-default files-import-alias* file-imports*]} options        

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
                (get @files-import-alias* import-file)))
             (get @file-imports* file))))
         {} files)

        file->type-literals
        (reduce
         (fn [m [file scheva-type-vs]]
           (assoc
            m file
            (map
             (fn [[schema-id _]]
               (let [{:keys [t-name literal jsdoc-literal export] :as t-options}
                     (get schema-id->type-desc schema-id)]
                 (->type-declaration-str
                  t-name literal jsdoc-literal
                  (merge t-options
                         {:export (if (some? export) export export-default)}))))
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

