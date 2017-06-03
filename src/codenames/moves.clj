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

;; Functions to Implement:
;; next-turn!
;; check-winning-condition
;; change-teams
;; current-team
