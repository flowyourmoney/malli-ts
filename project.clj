(defproject org.clojars.flowyourmoney "0.1.0-SNAPSHOT"
  :description "A library for generating TS type definitions from malli schemas"
  :url "https://flowyour.money/"

  :license {:name "MIT"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies
  [[metosin/malli "0.6.1"]
   [camel-snake-kebab "0.4.2"]]

  :source-paths
  ["src"]

  :repositories
  {"clojars" {:url "https://clojars.org/repo"
              :sign-releases false}})

