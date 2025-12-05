(ns core-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clj-diffmatchpatch :as diff]
            [uix.dom.server :as dom.server]
            [uix.core :refer [$]]
            [core-test-sample]))

(defn diff [s1 s2]
  (->> (diff/wdiff s1 s2)
       (map (fn [[op text]]
              (case op
                :delete (str "\033[37;41;1m" text "\033[0m")
                :insert (str "\033[37;42;1m" text "\033[0m")
                :equal text)))
       (str/join)))

(def css-out
  ".core-test-sample-14-5{margin:64px;padding:33px;border:1px solid #000;}.core-test-sample-14-5:hover{color:blue;background:yellow;width:var(--core-test-sample-20-0);}.core-test-sample-14-5:hover > div{border-radius:32px;}@media (max-width: 800px){.core-test-sample-14-5{color:yellow;width:17px;}.core-test-sample-14-5:hover{color:yellow;width:var(--core-test-sample-27-0);}}\n/*# sourceMappingURL=main.css.map */")

(def css-out-release
  (->> [".k1{margin:64px;padding:33px;border:1px solid #000;}"
        ".k1:hover{color:blue;background:yellow;width:var(--v2);}"
        ".k1:hover > div{border-radius:32px;}"
        "@media (max-width: 800px){.k1{color:yellow;width:17px;}.k1:hover{color:yellow;width:var(--v3);}}"
        "\n"
        "/*# sourceMappingURL=main.css.map */"]
       (str/join "")))

(defn after []
  (.delete (io/file "out/main.css"))
  (.delete (io/file "out/main.css.map"))
  (run! io/delete-file (reverse (file-seq (io/file ".styles"))))
  (run! io/delete-file (reverse (file-seq (io/file ".shadow-cljs"))))
  (run! io/delete-file (reverse (file-seq (io/file "out")))))

(defn exec [& cmd]
  (testing cmd
    (println "Running" (str "\"" (str/join " " cmd) "\""))
    (let [{:keys [exit out err]} (apply shell/sh cmd)]
      (is (= exit 0))
      (when-not (str/blank? err)
        (binding [*out* *err*]
          (println err)))
      (when-not (str/blank? out)
        (println out)))))

(deftest test-css-compilation
  (testing "generated CSS should match snapshot"
    (exec "clojure" "-A:dev" "-M" "-m" "shadow.cljs.devtools.cli" "compile" "test")
    (let [out-css (slurp "out/main.css")]
      (is (= css-out out-css)
          (diff css-out out-css)))
    (after))
  (testing "generated minified CSS should match snapshot"
    (exec "clojure" "-A:dev" "-M" "-m" "shadow.cljs.devtools.cli" "release" "test")
    (let [out-css (slurp "out/main.css")]
      (is (= css-out-release out-css)
          (diff css-out-release out-css)))
    (after)))

(def render-dir "server_render_test")

(deftest test-ssr-compat-between-jvm-and-js
  (doseq [^java.io.File f (reverse (file-seq (io/file render-dir)))]
    (when (.exists f)
      (.delete f)))
  (.mkdir (io/file render-dir))
  (.mkdir (io/file render-dir "html"))
  (.mkdir (io/file render-dir "markup"))
  (exec "clojure" "-A:dev" "-M" "-m" "shadow.cljs.devtools.cli" "compile" "test")
  (exec "node" "out/test.js")
  (let [cljs-html (slurp (str render-dir "/html/test.html"))
        clj-html (dom.server/render-to-string ($ core-test-sample/component))]
    (is (= cljs-html clj-html) (diff cljs-html clj-html)))
  (let [cljs-html (slurp (str render-dir "/markup/test.html"))
        clj-html (dom.server/render-to-static-markup ($ core-test-sample/component))]
    (is (= cljs-html clj-html) (diff cljs-html clj-html)))
  (after))

(defn -main [& args]
  (run-tests 'core-test))