(ns codenames.moves
  (:require [codenames.game :as game]
            [com.rpl.specter :refer :all]))

(defn in?
  "True if the collection contains the element."
  [collection element]
  (some #(= element %) collection))

(defn word-filterer [w {:keys [word]}]
  (= word w))

(defn valid-word? [game word]
  (let [words (select [ATOM :words ALL :word] game)]
    (in? words word)))

(defn revealed? [game word]
  (select-any [ATOM :words (filterer #(word-filterer word %)) ALL :revealed?] game))

(def hidden? (complement revealed?))

(defn reveal! [game word]
  (setval [ATOM :words (filterer #(word-filterer word %)) ALL :revealed?]
          true game))

(defn next-round! [game]
  (transform [ATOM :round] inc game))

(defn opposite-team [team]
  (if (= team :red)
    :blue
    :red))

(defn switch-teams! [game]
  (transform [ATOM :current-team] opposite-team game))

(defn next-turn! [game]
  (next-round! game)
  (switch-teams! game))

(defn win!
  "Makes the current team win the game."
  [game]
  (let [winner (select-any [ATOM :current-team] game)]
    (setval [ATOM :winning-team] winner game)))

(defn lose!
  "Makes the current team lose the game."
  [game]
  (let [loser (select-any [ATOM :current-team] game)
        winner (opposite-team loser)]
    (setval [ATOM :winning-team] winner game)))

(defn get-freqs [game]
  (let [words (select [ATOM :words ALL] game)
        get-attributes (juxt :identity :revealed?)]
    (->> words
         (map get-attributes)
         (frequencies))))

(defn winner?
  "If a GAME has a winner, return true. If not, return false."
  [game]
  (->> game
       (select-any [ATOM :winning-team])
       (some?)))

(defn- get-current-team
  [game]
  (select-any [ATOM :current-team] game))

(defn- get-id-of-word [game word]
  (select-any [ATOM :words (filterer #(word-filterer word %)) ALL :identity] game))

(defn move! [game word]
  {:pre [(valid-word? game word)
         (hidden? game word)
         (= false (winner? game))]}
  (reveal! game word)
  (let [current-team (get-current-team game)
        id (get-id-of-word game word)
        match-result (= id current-team) ;; Register whether they picked someone on their team, or on the other team.
        frqs (get-freqs game)
        blue-remaining (get frqs [:blue false])
        red-remaining (get frqs [:red false])]
    (cond (= id :assassin) (lose! game)
          (and (> blue-remaining 0) (> red-remaining 0)) ;; Check if there are remaining hidden cards for either team.
          ;; If they picked someone on their team, they can keep moving
          ;; If they picked someone from the other team, switch to make it the other team's turn.
          (if (true? match-result)
            game
            (next-turn! game))
          :else
          (if (true? match-result)
            ;; If the card picked was theirs, win!
            (win! game)
            ;; Otherwise, lose!
            (lose! game)))))
