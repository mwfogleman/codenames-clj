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

(defn- assassinate!
  "If the word is the assassin, the current team loses. If the word is not the assassin, reveal it."
  [game word]
  (let [id (select-any [ATOM :words (filterer #(word-filterer "RULER" %)) ALL :identity] g)]
    (if (= id :assassin)
      (lose! game)
      (reveal! game word))))

(defn winner?
  "If a GAME has a winner, return true. If not, return false."
  [game]
  (->> game
       (select-any [ATOM :winning-team])
       (some?)))

;; A player tries to make a move
;; If it's not a valid word, they can't make the move
;; If there's already a winner, they can't make the move
;; If they pick the assassin, they lose
;; Otherwise, reveal the card.
;; Register whether they picked someone on their team, or on the other team.
;; Check if there are remaining hidden cards for either team.
;; If not, set a winner. (depends who has no cards remaining - they may have revealed a card for the opposite team)
;; If they picked someone on their team, they can keep moving
;; If they picked someone from the other team, switch to make it the other team's turn.

(defn move! [game]
  {:pre [(valid-word? game word)
         (hidden? game word)]}
  ;; (let [winner? (fn [game] (true? (select-any [ATOM :winning-team nil?] game)))
  ;;       frqs (get-freqs game)
  ;;       assassin? (= 1 (get frqs [:assassin false]))
  ;;       blue-remaining (get frqs [:blue false])
  ;;       red-remaining (get frqs [:red false])]
  ;;   [{:frqs frqs
  ;;     :winner? (winner? game)
  ;;     :blue-remaining blue-remaining
  ;;     :red-remaining red-remaining}]
  ;;   )
  )
