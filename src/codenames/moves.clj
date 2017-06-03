(ns codenames.moves
  (:require [codenames.game :as game]
            [com.rpl.specter :refer :all]))

(defn in?
  "True if the collection contains the element."
  [collection element]
  (some #(= element %) collection))

(defn valid-word? [game word]
  (let [words (select [ATOM :words ALL :word] game)]
    (in? words word)))

(defn word-filterer [w {:keys [word]}]
  (= word w))

(defn reveal! [game word]
  {:pre [(valid-word? game word)]}
  (setval [ATOM :words (filterer #(word-filterer word %)) ALL :revealed?]
          true g))

