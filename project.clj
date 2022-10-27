(defproject org.clojars.flowyourmoney/malli-ts "0.3.0-SNAPSHOT"
  :description "A library for generating TS type definitions from malli schemas"
  :url "https://flowyour.money/"

  :license {:name "MIT"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies
  [[org.clojure/clojure "1.11.1"]
   [metosin/malli "0.9.2"]
   [camel-snake-kebab "0.4.2"]]

  :source-paths
  ["src"]

  :repositories
  {"clojars" {:url "https://clojars.org/repo"
              :sign-releases false}})
