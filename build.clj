(ns build
  "Build script for superficie.

   Usage:
     clojure -T:build jar      - Build JAR
     clojure -T:build install  - Install to local Maven repo
     clojure -T:build deploy   - Deploy to Clojars
     clojure -T:build clean    - Clean build artifacts"
  (:require [clojure.tools.build.api :as b]))

(def lib 'org.replikativ/superficie)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"
                                   :aliases [:release]})))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/replikativ/superficie"
                      :connection "scm:git:git://github.com/replikativ/superficie.git"
                      :developerConnection "scm:git:ssh://git@github.com/replikativ/superficie.git"
                      :tag (str "v" version)}
                :pom-data [[:description "Surface syntax for Clojure — bidirectional renderer for readable code exposition"]
                           [:url "https://github.com/replikativ/superficie"]
                           [:licenses
                            [:license
                             [:name "Eclipse Public License 2.0"]
                             [:url "https://www.eclipse.org/legal/epl-2.0/"]]]
                           [:developers
                            [:developer
                             [:id "whilo"]
                             [:name "Christian Weilbach"]
                             [:email "ch_weil@topiq.es"]]]]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println (str "JAR: " jar-file " (version " version ")")))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir})
  (println (str "Installed " lib " " version " to local Maven repo")))

(defn deploy
  "Deploy to Clojars.

   Requires environment variables:
     CLOJARS_USERNAME - your Clojars username
     CLOJARS_PASSWORD - your Clojars deploy token"
  [_]
  (jar nil)
  (let [dd (requiring-resolve 'deps-deploy.deps-deploy/deploy)]
    (println "Deploying to Clojars...")
    (dd {:installer :remote
         :artifact (b/resolve-path jar-file)
         :pom-file (b/pom-path {:lib lib :class-dir class-dir})})
    (println (format "Deployed %s version %s to Clojars" lib version))))
