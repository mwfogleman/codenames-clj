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

(defn check-winning-conditions [game]
  (let [winner? (fn [game] (true? (first (select [ATOM :winning-team nil?] game))))
        frqs (get-freqs game)
        assassin? (= 1 (get frqs [:assassin false]))
        blue-remaining (get frqs [:blue false])
        red-remaining (get frqs [:red false])]
    ;; frqs
    ;; {:winner? (winner? game)
    ;; :assassin? assassin?}
    [blue-remaining red-remaining]
    ;; if the assassin has been revealed, current team loses
    ))
