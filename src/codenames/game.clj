(ns codenames.game
  (:require [codenames.dictionaries :as dictionaries]))

;; 9 red 8 blue, or 9 blue 8 red
;; whoever has 9 goes first

(defn set-alliances
  "In each game, there should be: 1 :assassin, 9 of the starting team (e.g., :red), 8 of the next team (e.g., :blue), and 7 civilians (:neutral)."
  []
  (let [teams [:red :blue]
        order (shuffle teams)
        f (first order)
        s (second order)]
    (reduce concat [(repeat 9 f)
                    (repeat 8 s)
                    (repeat 7 :neutral)
                    [:assassin]])))

(defn prepare
  "Creates a new game of CODENAMES."
  []
  {:post [(= 25 (count %))]}
  (let [alls (set-alliances)
        coords (shuffle (for [x (range 5) y (range 5)] (vector x y)))
        mapper (fn [[id coord wd]] {:word wd
                                   :identity id
                                   :revealed? false
                                   :position coord})]
    (->> dictionaries/dictionary
         shuffle
         (take 25)
         (interleave alls coords)
         (partition 3)
         (map mapper))))

