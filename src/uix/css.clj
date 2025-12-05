(ns uix.css
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.source-map]
            [cljs.vendor.clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader.edn :as edn]
            [uix.css.lib :as css.lib])
  (:import (java.io File FileNotFoundException)))

(defn cljs? [env]
  (some? (:ns env)))

(defn -resolve [env v]
  (if (cljs? env)
    (ana-api/resolve env v)
    (meta (resolve env v))))

(defmacro debug [& body]
  `(binding [*out* *err*]
     ~@body))

;; var name -> ast node
(def defs (atom {}))

(defonce parse-def (get-method ana/parse 'def))

(defmethod ana/parse 'def [op env form name opts]
  (let [ast (parse-def op env form name opts)]
    (swap! defs assoc (:name ast) ast)
    ast))

(defn compile-rule [k v]
  (assert (not (or (symbol? v) (list? v))) "Value can't be a symbol or expression here, this is a bug")
  (str (name k) ":"
       (cond
         (and (number? v) (not (css.lib/unitless-prop k)))
         (str v "px")

         (keyword? v)
         (name v)

         :else v)
       ";"))

(defn compile-styles [class-name selector styles & {:keys [global?]}]
  (str (if global?
         (str/replace (name selector) "&" class-name)
         (str/replace (name selector) "&" (str "." class-name)))
       "{"
       (str/join "" (map #(apply compile-rule %) styles))
       "}"))

(defn styles-by-type [styles]
  (group-by #(let [k (-> % key name)]
               (cond
                 (str/starts-with? k "&") :blocks
                 (str/starts-with? k"@") :media
                 (= "global" k) :global
                 :else :self))
            styles))

(defn walk-styles-compile [class-name styles & {:keys [global?]}]
  (let [{:keys [self blocks media global]} (styles-by-type styles)]
    (str/join ""
      (concat
        ;; global styles
        (->> global
             (map second)
             (apply merge-with merge)
             (mapv (fn [[selector styles]]
                     (walk-styles-compile (name selector) styles :global? true))))
        ;; element styles
        (mapv #(apply compile-styles class-name (concat % [:global? global?])) (into [["&" self]] blocks))
        ;; element media queries
        ;; FIXME: media styles should be in the end of file
        ;; FIXME: keyframes
        (mapv (fn [[media styles]]
                (str media "{" (walk-styles-compile class-name styles :global? global?) "}"))
              media)))))

(def ^:dynamic *build-state*)

(defn release? []
  (-> *build-state*
      :mode
      (= :release)))

(defn root-path []
  ;; TODO: include build name dir
  (str ".styles/"
       (if (release?) "release" "dev")))

(defn styles-modules []
  (->> (file-seq (io/file (root-path)))
       (filter #(.isFile ^File %))))

(defn write-modules! [styles-reg]
  (doseq [[file styles] styles-reg]
    (let [path (str (root-path) "/" file ".edn")]
      (io/make-parents path)
      (spit path (str styles)))))

(defn write-source-map! [styles css-str output-to]
  (let [file-name (peek (str/split output-to #"/"))
        styles (->> styles
                    (reduce-kv (fn [ret class v]
                                 (assoc ret class (assoc v :css-loc (.indexOf ^String css-str ^String class))))
                               {}))
        sm (->> (vals styles)
                (reduce (fn [ret {:keys [file line column css-loc]}]
                          (assoc-in ret [file (dec line) column] [{:gline 0 :gcol css-loc}]))
                        {}))
        sources (->> (vals styles)
                     (map (fn [{:keys [file]}]
                            [file (-> file io/resource slurp)]))
                     (into #{})
                     vec)
        sources-content (map second sources)
        source-files (map first sources)
        sm (into (cljs.source-map/encode* sm {:file file-name
                                              :sources-content sources-content})
                 {"sources" source-files})]
    (spit (str output-to ".map") (json/write-str sm :escape-slash false))))

(defn write-bundle! [[output-name styles] output-dir]
  (let [styles (into {} styles)
        styles-strs (->> styles
                         (sort-by (comp #(->> (str/split % #"-") (take-last 2) (str/join "."))
                                        key))
                         (map (comp :css-str val)))
        sm-path (str output-name ".map")
        output-to (str output-dir "/" output-name)
        out (str (str/join "" styles-strs)
                 "\n/*# sourceMappingURL=" sm-path " */")]
    (write-source-map! styles out output-to)
    (try
      (when (not= (slurp output-to) out)
        (spit output-to out))
      (catch FileNotFoundException e
        (spit output-to out)))))

(defn write-bundles! [state {:keys [output-dir]}]
  (let [output-dir (or output-dir (->> state :shadow.build/config :output-dir))
        build-sources (->> (:build-sources state)
                           (map second)
                           (filter string?)
                           (into #{}))
        modules (->> (vals (:shadow.build.modules/modules state))
                     (map (fn [{:keys [module-name sources]}]
                            {:output-name (str/replace module-name #"\.js$" ".css")
                             :sources (->> sources
                                           (filter (comp #{:shadow.build.classpath/resource} first))
                                           (keep (comp :ns (:sources state)))
                                           (into #{}))})))
        used (->> (styles-modules)
                  (filter #(-> (.getPath ^File %)
                               (str/replace #"^\.styles\/(dev|release)/" "")
                               (str/replace #"\.edn$" "")
                               build-sources)))
        styles (->> used
                    (map (comp edn/read-string slurp))
                    (apply merge)
                    (reduce-kv (fn [ret class styles]
                                 (assoc ret class (->> (walk-styles-compile class (:styles styles))
                                                       (assoc styles :css-str))))
                               {}))
        style-modules (->> styles
                           (group-by (fn [[_ {:keys [ns]}]]
                                       (->> modules
                                            (reduce #(when (contains? (:sources %2) ns)
                                                       (reduced (:output-name %2)))
                                                    nil)))))]
    (.mkdirs (io/file output-dir))
    (run! #(write-bundle! % output-dir) style-modules)))

(defn- build-state->styles-reg [{:keys [compiler-env]}]
  (->> (:cljs.analyzer/namespaces compiler-env)
       vals
       (map :uix/css)
       (apply merge)))

(defn write-styles! [state config]
  (binding [*build-state* state]
    (write-modules! (build-state->styles-reg state))
    (write-bundles! state config)))

(defn eval-symbol [env v]
  (if-not (cljs? env)
    (if-let [var (resolve env v)]
      (if (or (number? @var) (string? @var))
        @var
        ::nothing)
      (or (some-> (env v) .init .eval)
          ::nothing))
    (let [ast (ana-api/resolve env v)]
      (cond
        (and (= :local (:op ast))
             (-> ast :init :op (= :const)))
        (-> ast :init :val)

        (= :var (:op ast))
        (let [ast (@defs (:name ast))]
          (when (-> ast :init :op (= :const))
            (-> ast :init :val)))

        :else ::nothing))))

(def evaluators
  {'cljs.core/inc inc
   'cljs.core/dec dec
   'cljs.core/+ +
   'cljs.core/- -
   'cljs.core/* *
   'cljs.core// (comp float /)
   'cljs.core/str str

   'inc inc
   'dec dec
   '+ +
   '- -
   '* *
   '/ (comp float /)
   'str str})

;; TODO: add a walker for well known forms: when, if, etc

(declare eval-css-value)

(defn eval-expr [env [f & args :as expr]]
  (cond
    (symbol? f)
    (if-let [eval-fn (->> f (-resolve env) :name evaluators)]
      (let [args (map #(eval-css-value env %) args)]
        (if (every? (complement #{::nothing}) args)
          (apply eval-fn args)
          ::nothing))
      ::nothing)

    :else ::nothing))

(defn eval-value [env v]
  (if-not (cljs? env)
    (if (or (number? v) (string? v))
      v
      ::nothing)
    (ana-api/no-warn
      (let [ast (ana-api/analyze env v)]
        (if (= :const (:op ast))
          (:val ast)
          ::nothing)))))

(def release-counter (atom 0))

(defn dyn-var-name [env v line]
  (if (release?)
    (str "--v" (swap! release-counter inc))
    (let [v (cond-> v
              (and (list? v) (= 'clojure.core/deref (first v)))
              second)
          {:keys [file ns]
           :or {file *file* ns *ns*}}
          (if (symbol? v)
            (-resolve env v)
            (meta v))
          column 0
          ns (if ns
               (-> (str ns) (str/replace #"\." "-"))
               (-> file
                   (str/replace #"\.clj(s|c)?$" "")
                   (str/replace #"(/|_)" "-")))]
      (str "--" ns "-" line "-" column))))

(defn eval-css-value [env v]
  (or (cond
        (symbol? v) (eval-symbol env v)
        (list? v) (eval-expr env v)
        :else (eval-value env v))
      ::nothing))

(def ^:dynamic *global-context?* false)

(defn walk-map [f m]
  (letfn [(walk [x]
            (if (map? x)
              (into {} (->> x (map (fn [[k v]]
                                     (if-not (and (= :global k) (map? v))
                                       (f [k (walk v)])
                                       (binding [*global-context?* true]
                                         (f [k (walk v)])))))))
              x))]
    (walk m)))

(defn- env-with-loc [env form]
  (let [loc (select-keys (meta form) [:line :column])]
    (cond-> env (seq loc) (into loc))))

(defn find-dyn-styles [styles env line]
  (let [dyn-input-styles (atom {})
        line (atom line)]
    [(walk-map (fn [[k v]]
                 (swap! line inc)
                 (if-not (or (symbol? v) (list? v) (instance? clojure.lang.Cons v))
                   [k v]
                   (let [ret (eval-css-value env v)]
                     (when (and *global-context?* (= ::nothing ret))
                       (ana/warning ::global-styles-dynamic-vars (env-with-loc env v) {}))
                     (if (= ::nothing ret)
                       (let [var-name (dyn-var-name env v @line)]
                         (swap! dyn-input-styles assoc var-name `(uix.css.lib/interpret-value ~k ~v))
                         [k (str "var(" var-name ")")])
                       [k ret]))))
               styles)
     @dyn-input-styles]))

(let [env (atom {})]
  (defn get-env []
    (or env/*compiler* env)))

(defn make-styles [styles env form]
  (let [ns (or (-> env :ns :name) (ns-name *ns*))
        file (or (-> env :ns :meta :file) *file*)
        {:keys [line column]} (meta form)
        class (if (release?)
                (str "k" (swap! release-counter inc))
                (str (-> ns (str/replace "." "-")) "-" line "-" column))
        [evaled-styles dyn-input-styles] (find-dyn-styles styles env line)]
    (swap! (get-env) assoc-in [:cljs.analyzer/namespaces ns :uix/css file class]
           {:styles evaled-styles
            :file file
            :ns ns
            :line line
            :column column
            :dyn-input-styles dyn-input-styles})
    {:uixCss {:class class
              :vars dyn-input-styles}}))

(defmacro css [& styles]
  (if (cljs? &env)
    (binding [*build-state* (:shadow.build.cljs-bridge/state @env/*compiler*)]
      (let [styles (->> styles
                        (mapv (fn [v]
                                (if (map? v)
                                  `(cljs.core/array ~(make-styles v &env &form))
                                  `(let [v# ~v]
                                     (if (map? v#)
                                       (cljs.core/array v#)
                                       v#))))))]
        `(.concat ~@styles)))
    (binding [*build-state* (atom {})]
      (let [styles (->> styles
                        (mapv (fn [v]
                                (if (map? v)
                                  [(make-styles v &env &form)]
                                  `(let [v# ~v]
                                     (if (map? v#)
                                       [v#]
                                       v#))))))]
        `(vec (concat ~@styles))))))


(defn hook
  {:shadow.build/stage :compile-finish}
  [build-state & [config]]
  (write-styles! build-state config)
  build-state)

(defmethod ana/error-message ::global-styles-dynamic-vars [_ _]
  "Global styles can't have dynamic values")

(defmacro load-before [ns promise]
  (let [asset-path (->> (:shadow.build.cljs-bridge/state @env/*compiler*)
                        :shadow.build/config
                        :asset-path)
        path (->> (:shadow.build.cljs-bridge/state @env/*compiler*)
                  :shadow.build.modules/modules
                  vals
                  (some #(when (contains? (set (:entries %)) ns)
                           (str asset-path "/" (str/replace (:module-name %) #"\.js$" ".css")))))]
    `(-> (load-stylesheet ~path)
         (.then (fn [] ~promise)))))

(comment
  (require '[uix.core :refer [$]])
  (require 'uix.dom.server)

  (def xx (atom 90))
  (let [x (atom 1)]
    (css {:font-size "14px" :flex (+ @xx 89)})
    (uix.dom.server/render-to-string
      ($ :div.flex.flex-col.items-center {:style (css {:font-size "14px" :flex (+ @xx 89)})}
         ($ :ul.flex.gap-2.text-sm.py-1.font-medium)))))