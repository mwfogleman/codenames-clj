(ns codenames.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def sample-word
  "Exemplifies the CODENAME data structure. A vector of 25 of these words constitutes a game." ;; add in atoms for state?
  {:word "EAGLE"
   ;; In each game, there should be:
   ;; 1 :assassin
   ;; 9 :red
   ;; 8 :blue
   ;; 7 :neutral
   :identity :assassin
   ;; Each word should have an x, y coordinate from [0 0] to [4 3]
   :position [0 1]
   ;; Each word should have a :revealed? status, true or false, default false.
   :revealed? false
   })

(def dictionary (-> "resources/original.txt" slurp str/split-lines))

;; 9 red 8 blue, or 9 blue 8 red
;; whoever has 9 goes first

(defn set-alliances
  "In each game, there should be: 1 :assassin, 9 of the starting team (e.g., :red), 8 of the next team (e.g., :blueed), and 7 civilians (:neutral)."
  []
  (let [teams [:red :blue]
        order (shuffle teams)
        f (first order)
        s (second order)]
    (reduce concat [(repeat 9 f)
                    (repeat 8 s)
                    (repeat 7 :neutral)
                    [:assassin]])))

(defn prepare-game
  "Creates a new game of CODENAMES."
  []
  (let [alliances (set-alliances)
        coordinates (shuffle (for [x (range 5) y (range 5)] (vector x y)))
        mapper (fn [[id coord wd]] {:word wd
                                   :identity id
                                   :revealed? false
                                   :position coord})]
    (->> dictionary
         shuffle
         (take 25)
         (interleave alliances coordinates)
         (partition 3)
         (map mapper))))

(defn hidden?
  [m]
  (= (:revealed? m) false))

(defn reveal-word
  [map]
  {:pre [(hidden? map)]}
  (assoc map :revealed? true))

(def game (prepare-game))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
