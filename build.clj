(ns build
  "Tasks for compiling and building artifacts for deployment.

  Each task will check to see if its output has already been computed, and if it
  has, it will not execute. To resolve this, just run [[clean]] first.

  Tasks can be chained with the default function, [[run-tasks]], which runs each
  of a sequence of tasks in order, passing the options map from one to the next.

  Each task takes arguments in its options of a form `:task-name/option` so that
  when the options map is passed between them no conflicts will arise.

  While these tasks do construct artifacts, they do not provide for making
  deployments. Use the `:deploy` alias to deploy to clojars."
  (:require
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(def lib-coord 'org.suskalo/coffi)
(def version (format "0.3.%s" (b/git-count-revs nil)))

(def resource-dirs ["resources/"])

(def source-dirs ["src/clj/"])
(def java-source-dirs ["src/java/"])

(def c-test-dirs ["test/c/"])

(def target-dir "target/")
(def class-dir (str target-dir "classes/"))

(def basis (b/create-basis {:project "deps.edn"}))

(def jar-file (str target-dir "coffi.jar"))
(def test-c-library (str target-dir "ffi_test.so"))

(defn clean
  "Deletes the `target/` directory."
  [opts]
  (b/delete {:path target-dir})
  opts)

(defn- exists?
  "Checks if a file composed of the given path segments exists."
  [& path-components]
  (.exists ^java.io.File (apply io/file path-components)))

(defn compile-java
  "Compiles java classes required for interop."
  [opts]
  (.mkdirs (io/file class-dir))
  (b/process {:command-args ["javac" "--add-modules=jdk.incubator.foreign"
                             "src/java/coffi/ffi/Loader.java"
                             "-d" class-dir
                             "-target" "17"
                             "-source" "17"]})
  opts)

(defn- write-pom
  "Writes a pom file if one does not already exist."
  [opts]
  (when-not (exists? (b/pom-path {:lib lib-coord
                                  :class-dir class-dir}))
    (b/write-pom {:basis basis
                  :class-dir class-dir
                  :lib lib-coord
                  :version version
                  :scm {:url "https://github.com/IGJoshua/coffi"
                        :connection "scm:git:git://github.com/IGJoshua/coffi.git"
                        :developerConnection "scm:git:ssh://git@github.com/IGJoshua/coffi.git"
                        :tag (str "v" version)}
                  :src-dirs source-dirs})
    (b/copy-file {:src (b/pom-path {:lib lib-coord
                                    :class-dir class-dir})
                  :target (str target-dir "pom.xml")}))
  opts)

(defn pom
  "Generates a `pom.xml` file in the `target/classes/META-INF` directory.
  If `:pom/output-path` is specified, copies the resulting pom file to it."
  [opts]
  (write-pom opts)
  (when-some [path (:output-path opts)]
    (b/copy-file {:src (b/pom-path {:lib lib-coord
                                    :class-dir class-dir})
                  :target path}))
  opts)

(defn- copy-resources
  "Copies the resources from the [[resource-dirs]] to the [[class-dir]]."
  [opts]
  (b/copy-dir {:target-dir class-dir
               :src-dirs resource-dirs})
  opts)

(defn jar
  "Generates a `coffi.jar` file in the `target/` directory.
  This is a thin jar including only the sources."
  [opts]
  (write-pom opts)
  (compile-java opts)
  (copy-resources opts)
  (when-not (exists? target-dir jar-file)
    (b/copy-dir {:target-dir class-dir
                 :src-dirs source-dirs})
    (b/jar {:class-dir class-dir
            :jar-file jar-file}))
  opts)

(defn compile-test-library
  "Compiles the C test code for running the tests."
  [opts]
  (let [c-files (->> c-test-dirs
                     (map io/file)
                     (mapcat file-seq)
                     (filter #(.isFile %))
                     (map #(.getAbsolutePath %)))]
    (.mkdirs (io/file target-dir))
    (b/process {:command-args (concat ["clang" "-fpic" "-shared"]
                                      c-files
                                      ["-o" test-c-library])}))
  opts)

(defn run-tasks
  "Runs a series of tasks with a set of options.
  The `:tasks` key is a list of symbols of other task names to call. The rest of
  the option keys are passed unmodified."
  [opts]
  (binding [*ns* (find-ns 'build)]
    (reduce
     (fn [opts task]
       ((resolve task) opts))
     opts
     (:tasks opts))))
