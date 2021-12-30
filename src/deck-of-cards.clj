(ns deck-of-cards.core
  (:gen-class))

(def suits ["♠" " ♥ " "♣" "♦"])
(def faces ["J" "Q" "K" "A"])
(def numbers (map #(+ 2 %) (range 9)))
(def card-values (concat numbers faces))

(defn card 
  [value suit]
  {:value value
   :suit suit})

(def deck 
  (for [suit suits
        value card-values]
    (card value suit)))

(defn get-card
  "draw random card from deck"
  []
  (nth deck (rand-int 52)))
  
(defn -main
  "get deck of cards"
  []
  (prn deck)

(comment 
  (card card-values " ♥ ")
  (get-card)
         )
