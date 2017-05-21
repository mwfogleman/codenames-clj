(ns codenames.core
  (:require [codenames.game :as game]
            [codenames.dictionaries :as dictionaries]
            [codenames.words :as words])
  (:gen-class))

(def a-game (game/prepare))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
