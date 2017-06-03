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
  {:pre [(valid-word? game word)]}
  (select [ATOM :words (filterer #(word-filterer word %)) ALL :revealed? FIRST] game))

(def hidden? (complement revealed?))

(defn reveal! [game word]
  {:pre [(valid-word? game word)
         (hidden? game word)]}
  ;; check that the word hasn't already been revealed
  (setval [ATOM :words (filterer #(word-filterer word %)) ALL :revealed?]
          true g))

(defn next-round! [game]
  (transform [ATOM :round] inc game))

(defn switch-teams! [game]
  (letfn [(switcher [team] (if (= team :red)
                             :blue
                             :red))]
    (transform [ATOM :current-team] switcher game)))

(defn next-turn! [game]
  (next-round! game)
  (switch-teams! game))

(defn win! [game]
  (let [current-team (first (select [ATOM :current-team] game))]
    (setval [ATOM :winning-team] current-team game)))

;; Functions to Implement:
;; check-winning-condition
