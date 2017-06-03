(ns codenames.game
  (:require [codenames.dictionaries :as dictionaries]
            [clj-time.core :as t]))

(def teams [:red :blue])

(defn set-alliances
  "In each game, there should be: 1 :assassin, 9 of the starting team (e.g., :red), 8 of the next team (e.g., :blue), and 7 civilians (:neutral). Return a sequence with those amounts of the keywords, as well as a map that says who the starting team is."
  []
  (let [[fst snd] (shuffle teams)
        m {:starting-team fst
           :current-team fst}]
    (cons m (reduce concat [(repeat 9 fst)
                            (repeat 8 snd)
                            (repeat 7 :neutral)
                            [:assassin]]))))

(defn get-words
  []
  (->> dictionaries/dictionary
       shuffle
       (take 25)))

(defn prepare-game
  "Creates a new game of CODENAMES."
  []
  (let [[alliance-map & alliances] (set-alliances)
        metadata-init {:winning-team nil
                       :id (str (gensym))
                       :created-at (t/now)
                       :round 0}
        metadata (merge alliance-map metadata-init)
        ;; I think I can do coordinates by order in the sequence, with partition-by or even without it
        ;; coords (shuffle (for [x (range 5) y (range 5)] (vector x y))) 
        mapper (fn [[id wd]] {:word wd
                             :identity id
                             :revealed? false
                             ;; :position coord
                             })]
    (->> (get-words)
         (interleave alliances)
         (partition 2)
         (map mapper)
         (hash-map :words)
         (into metadata)
         (atom))))

