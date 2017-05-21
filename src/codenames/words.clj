(ns codenames.words)

(def identities #{:assassin :red :blue :neutral})
(def coordinates [0 1 2 3 4])

(defn make
  [word identity revealed [x y :as coordinate]]
  {:pre [(string? word)
         (contains? identities identity)
         (instance? Boolean revealed)
         (contains? coordinates x)
         (contains? coordinates y)]}
  {:word word
   :identity identity
   :revealed revealed
   :position coordinate})

(defn hidden?
  [m]
  (= (:revealed? m) false))

(defn reveal
  [map]
  {:pre [(hidden? map)]}
  (assoc map :revealed? true))
