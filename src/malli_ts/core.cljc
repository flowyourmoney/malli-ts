(ns malli-ts.core
  (:require [malli-ts.ast :refer [->ast]]
            [malli-ts.core-schema :as core-schemas]
            [malli.core :as m]
            [malli.registry :as mr]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as string]
            [clojure.set :as set]
            #?(:cljs ["path" :as path])))

#?(:clj
   (defn- get-path
     [f]
     (java.nio.file.Paths/get f (into-array String []))))

(defn- import-path-relative
  [f1 f2]
  (if-let [absolute (get f2 :absolute)]
    absolute
    #?(:cljs (path/relative (path/dirname f1) f2)
       :clj (let [p1 (.resolve (get-path f1)
                               ;; quick way to get the parent directory if import has .d.ts extension
                               (if-not (re-matches #"[.]" f1) ".." "."))
                  p2 (get-path f2)]
              (str "./"  (.relativize p1 p2))))))

(defn- -schema-properties
  [?schema options]
  (if ?schema
    (-> (if (= (m/type ?schema options) ::m/val)
          (-> ?schema (m/children options) first)
          ?schema)
        (m/properties options))
    nil))

(defn- -dispatch-parse-ast-node
  [node options]
  (cond
    (if-let [{:keys [schema]} node]
      (-> schema (-schema-properties options) ::external-type)
      nil)
    :external-type
    
    (not (some? node)) :nil-node
    (:$ref node) :$ref
    (:type node) [:type (:type node)]
    (:union node) :union
    (:intersection node) :intersection
    (some? (:const node)) :const
    :else [:type :any]))

(defmulti ^:private -parse-ast-node
#'-dispatch-parse-ast-node)

(defn parse-ast-node
([node options]
 (-parse-ast-node node options))
([node]
 (-parse-ast-node node {})))

(defmethod -parse-ast-node :$ref
[{:keys [$ref] :as node} {:keys [deref-type
                                 schema-id->type-options
                                 files-import-alias*
                                 file-imports*
                                 file]
                          :as options}]
(if (or (= deref-type $ref) (not (get schema-id->type-options $ref)))
  (-parse-ast-node
   (or (get-in node [:definitions $ref])
       (->ast (:schema node)))
   (dissoc options :deref-type))
  (let [ref-file (get-in schema-id->type-options [$ref :file])
        import-alias (or (get @files-import-alias* ref-file)
                         (get
                          (swap!
                           files-import-alias* assoc ref-file
                           (as-> ref-file $
                             (string/split $ #"[./]")
                             (if (= (take-last 2 $) ["d" "ts"])
                               (drop-last 2 $)
                               $)
                             (string/join "_" $)))
                          ref-file))
        ref-type-name (get-in schema-id->type-options [$ref :t-name])
        same-file? (= file ref-file)]
    (when-not same-file?
      (swap! file-imports* update file set/union #{ref-file}))
    (str (if-not same-file? (str import-alias ".") nil) ref-type-name))))

(defmethod -parse-ast-node [:type :number] [_ _] "number")
(defmethod -parse-ast-node [:type :string] [_ _] "string")
(defmethod -parse-ast-node [:type :boolean] [_ _] "boolean")
(defmethod -parse-ast-node [:type :any] [_ _] "any")
(defmethod -parse-ast-node [:type :undefined] [_ _] "undefined")
(defmethod -parse-ast-node :const [{:keys [const]} options]
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

(defmethod -parse-ast-node :union [{items :union} options]
  (str "(" (string/join "|" (map #(-parse-ast-node % options) items)) ")"))

(defmethod -parse-ast-node :intersection [{items :intersection} options]
  (str "(" (string/join "&" (map #(-parse-ast-node % options) items)) ")"))

(defmethod -parse-ast-node [:type :object] [{:keys [properties
                                                    optional
                                                    index-signature]}
                                            {:keys [default-to-camel-case]
                                             :as options}]
  (let [idx-sign-literal (if index-signature
                           (str "[k:" (-parse-ast-node (first index-signature) options) "]:"
                                (-parse-ast-node (second index-signature) options))
                           nil)
        properties-literal (if-not (empty? properties)
                             (string/join
                              ","
                              (map (fn [[k v]]
                                     (let [property-name (if default-to-camel-case
                                                           (csk/->camelCase (name k))
                                                           (name k))]
                                       (str \" property-name \"
                                            (if (contains? optional k) "?" nil) ":"
                                            (-parse-ast-node v options))))
                                   properties))
                             nil)]
    (str "{" (string/join "," (filter (comp not string/blank?)
                                      [idx-sign-literal properties-literal]))
         "}")))

(comment
  (-parse-ast-node
   (->ast [:map [:a :int] [:b {:optional true} :string]])
   {})
  (-parse-ast-node
   (->ast [:map [:a :int] [:b {"optional" true} :string]])
   {}))

(defmethod -parse-ast-node :external-type [{:keys [schema]}
                                           {:keys [deref-type
                                                   file
                                                   files-import-alias*
                                                   file-imports*
                                                   schema-id->type-options]
                                            :as options}]
  (let [{:keys [::external-type ::t-path ::t-alias]} (-schema-properties schema options)

        {:keys [external-type t-path t-alias]
         :or {external-type external-type t-path t-path t-alias t-alias}}
        (get schema-id->type-options deref-type)

        t-path-str (or (:absolute t-path) t-path)
        is-imported-already (if t-path-str (@file-imports* t-path) nil)
        canonical-alias (if t-path-str (get @files-import-alias* t-path-str) nil)
        import-alias (or canonical-alias
                         (cond
                           t-alias t-alias
                           t-path-str (as-> t-path-str $
                                        (string/replace $ #"\.d\.ts" "")
                                        (string/split $ #"[./]")
                                        (string/join "-" $)
                                        (csk/->snake_case $))
                           :else nil))]
    (when (and t-path-str (not is-imported-already))
      (swap! file-imports* update file set/union #{t-path}))
    (when (and import-alias (not canonical-alias))
      (swap! files-import-alias* assoc t-path-str import-alias))
    (str (if import-alias (str import-alias ".") nil) external-type)))

(defn- letter-args
  ([letter-arg]
   (if letter-arg
     (let [letter-count (.substring letter-arg 1)
           next-count (if-not (empty? letter-count)
                        (-> letter-count
                            #?(:cljs js/Number
                               :clj Integer/parseInt)
                            inc)
                        1)
           char-code #?(:cljs (.charCodeAt letter-arg 0)
                        :clj (int (.charAt letter-arg 0)))
           z? (= char-code 122)
           next-letter (if-not z?
                         (let [next-char-code (inc char-code)]
                           #?(:cljs (.fromCharCode js/String next-char-code)
                              :clj (char next-char-code)))
                         "a")
           next-letter-arg (str next-letter (if z? next-count letter-count))]
       (cons next-letter-arg
             (lazy-seq (letter-args next-letter-arg))))
     (cons "a" (lazy-seq (letter-args "a")))))
  ([] (letter-args nil)))

(comment (->> (take 69 (letter-args)) (take-last 5)))

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
   (->ast [:=> [:cat :string :int] [:map [:a :int] [:b :string]]]) {}))

(defn import-literal
  [from alias]
  (str "import * as " alias " from " \' from \' \;))

(comment
  #?(:cljs (import-literal
            (path/relative (path/dirname "flow/person/index.d.ts") "flow/index.d.ts")
            "flow")))

(defn ->type-declaration-str
  [type-name literal jsdoc-literal options]
  (let [{:keys [export declare]} options]
    (str (if jsdoc-literal (str jsdoc-literal \newline) nil)
         (if export "export " nil)
         (if declare "var " "type ")
         type-name (if declare ": " " = ") literal ";")))

(defn -dispatch-provide-jsdoc [jsdoc-k _ _ _] jsdoc-k)

(defmulti provide-jsdoc #'-dispatch-provide-jsdoc)

#_{:clj-kondo/ignore [:unused-binding]}
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
         "\n */")
    nil))

(comment
  (println (-jsdoc-literal [["schema" (str '[:map-of any?])]
                            ["author" "Mr. Poopybutthole"]])))

(m/=> transform-parse-files-input-into-schema-id->type-options
      [:=>
       [:catn
        [:file->schema-type-vectors core-schemas/file->schema-type-vectors]
        [:options core-schemas/parse-files-options]]
       core-schemas/schema-id->type-options])

(defn- transform-parse-files-input-into-schema-id->type-options
  [file->schema-type-vectors options]
  (reduce
   (fn [m [file schema-type-vectors]]
     (merge m (reduce
               (fn [m schema-type-vector]
                 (let [[schema-id type-options] (if (seqable? schema-type-vector)
                                                  schema-type-vector
                                                  [schema-type-vector])
                       schema-type-options
                       (into {}
                             (comp
                              (filter (fn [[k _]] (= (namespace k) "malli-ts.core")))
                              (map (fn [[k v]] [(-> k name keyword) v])))
                             (m/properties (m/deref schema-id options)))
                       type-options (merge  type-options
                                            schema-type-options)
                       type-options (if-not (:t-name type-options)
                                      (assoc type-options :t-name (csk/->snake_case (name schema-id)))
                                      type-options)]
                   (assoc m schema-id (assoc type-options :file file))))
               {} schema-type-vectors)))
   {} file->schema-type-vectors))

(m/=> assoc-literals
      [:=>
       [:catn
        [:file->schema-type-vectors core-schemas/file->schema-type-vectors]
        [:options core-schemas/assoc-literals-options]]
       core-schemas/schema-id->type-options])

(defn- assoc-literals
  [file->schema-type-vectors
   {:keys [schema-id->type-options jsdoc-default] :as options}]
  (reduce
   (fn [m [file schema-type-vectors]]
     (reduce
      (fn [m schema-type-vector]
        (let [[schema-id type-options] (if (seqable? schema-type-vector)
                                         schema-type-vector
                                         [schema-type-vector])
              {:keys [jsdoc] :as type-options} (merge type-options (get m schema-id))
              literal
              (-parse-ast-node
               (->ast schema-id options)
               (merge options
                      {:deref-type schema-id 
                       :file file
                       :t-options type-options}))
              jsdoc-literal
              (->> (concat jsdoc-default jsdoc)
                   (map #(provide-jsdoc % schema-id type-options options))
                   -jsdoc-literal)]
          (-> m
              (assoc-in [schema-id :literal] literal)
              (assoc-in [schema-id :jsdoc-literal] jsdoc-literal))))
      m schema-type-vectors))
   schema-id->type-options file->schema-type-vectors))

(m/=> aggregate-into-file-maps
      [:=>
       [:catn
        [:file->schema-type-vectors core-schemas/file->schema-type-vectors]
        [:options core-schemas/assoc-literals-options]]
       [:catn
        [:file->import-literals [:map-of string? [:sequential string?]]]
        [:file->type-literals [:map-of string? [:sequential string?]]]]])

(defn- aggregate-into-file-maps
  [file->schema-type-vectors
   {:keys [schema-id->type-options export-default files-import-alias* file-imports*]}]
  (reduce
   (fn [[m-import m-type] [file scheva-type-vectors]]
     [(assoc
       m-import file
       (map
        (fn [import-file]
          (import-literal
           (import-path-relative file import-file)
           (get @files-import-alias* (or (:absolute import-file) import-file))))
        (get @file-imports* file)))
      (assoc
       m-type file
       (map
        (fn [schema-type-vector]
          (let [[schema-id _] (if (seqable? schema-type-vector)
                                schema-type-vector
                                [schema-type-vector])
                {:keys [t-name literal jsdoc-literal export] :as t-options}
                (get schema-id->type-options schema-id)
                t-name (or t-name
                           (munge (name schema-id)))]
            (->type-declaration-str
             t-name literal jsdoc-literal
             (merge t-options
                    {:export (if (some? export) export export-default)}))))
        scheva-type-vectors))])
   [{} {}] file->schema-type-vectors))

(m/=> parse-files
      [:=>
       [:catn
        [:file->schema-type-vectors core-schemas/file->schema-type-vectors]
        [:options core-schemas/assoc-literals-options]]
       core-schemas/parse-files-return])

(defn parse-files
  [file->schema-type-vectors options]
  (let [{:keys [registry use-default-schemas files-import-alias]
         :or {registry {}, use-default-schemas true, files-import-alias {}}} options

        options (merge options {:registry (if use-default-schemas
                                            (merge registry (m/default-schemas))
                                            registry)})

        schema-id->type-options
        (transform-parse-files-input-into-schema-id->type-options
         file->schema-type-vectors options)

        ;; Normalize symbols to strings
        files-import-alias (into {} (map (fn [[k v]] [(str k) (str v)]) files-import-alias))

        options (merge options {:schema-id->type-options schema-id->type-options
                                :file-imports* (atom {})
                                :files-import-alias* (atom files-import-alias)})

        schema-id->type-options
        (assoc-literals file->schema-type-vectors options)

        options (assoc options :schema-id->type-options schema-id->type-options)

        [file->import-literals file->type-literals]
        (aggregate-into-file-maps file->schema-type-vectors options)

        file-contents
        (reduce (fn [m [file]]
                  (assoc
                   m (str file (if-not (re-matches #".*\.d\.ts$" file) ".d.ts" nil))
                   (let [import (string/join "\n" (get file->import-literals file))
                         types (string/join "\n" (get file->type-literals file))]
                     (str (if-not (string/blank? import) (str import "\n\n") nil)
                          types))))
                {} file->schema-type-vectors)]
    file-contents))

(defn parse-matching-schemas
  "Only applicable to qualified schema-types and not defined in malli.core"
  {:arglists '([options]
               [pred options])}
  ([pred {:keys [registry transform] :as options}]
   (let [schemas (into []
                       (comp (filter (fn [[k s]]
                                       (and (qualified-keyword? k)
                                            (not= "malli.core" (namespace k))
                                            (pred k s))))
                             (map (fn [[k _]] [k {}]))
                             (map (or transform identity)))
                       (mr/schemas (mr/composite-registry registry m/default-registry)))
         parse-files-arg (persistent!
                          (reduce
                           (fn [acc [k opts]]
                             (let [file-name (csk/->snake_case (namespace k))]
                               (if-let [asdf (get acc file-name)]
                                 (assoc! acc file-name (conj asdf [k opts]))
                                 (assoc! acc file-name [[k opts]]))))
                           (transient {}) schemas))]
     (parse-files parse-files-arg options)))
  ([options]
   (parse-matching-schemas (constantly true) options))
  ([]
   (parse-matching-schemas (constantly true) {})))

(defn parse-ns-schemas
  ([ns-coll options]
   (parse-matching-schemas
    (let [ns-set (into #{} (map str) ns-coll)]
      (fn [k _]
        (let [k-ns (namespace k)]
          (contains? ns-set k-ns))))
    options))
  ([ns-coll]
   (parse-ns-schemas ns-coll {})))

(defn external-type
  [external-type-name & {:keys [import-path import-alias type-name schema]}]
  (letfn [(?assoc [m k v] (if v (assoc m k v) m))]
    [(or schema any?)
     (-> (?assoc {} ::external-type external-type-name)
         (?assoc ::t-name type-name)
         (?assoc ::t-path (cond
                            (nil? import-path) nil
                            (map? import-path) import-path
                            :else {:absolute import-path}))
         (?assoc ::t-alias import-alias))]))
