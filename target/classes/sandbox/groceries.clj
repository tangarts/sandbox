(ns sandbox.groceries
  (:require [clojure.set :as set]))

;; Grocery Example 


(defprotocol Append
  "This protocol lets us add things to the end of a collection."
  (append [coll x]
    "Appends x to the end of collection coll."))

(extend-protocol Append
  clojure.lang.IPersistentVector
  (append [v x]
    (conj v x))

  clojure.lang.Sequential
  (append [v x]
    (concat v (list x)))

  clojure.lang.PersistentHashSet
  (append [s x]
    (conj s x))
  nil
  (append [v x]
    [x]))

; Add a checked-off field to the GroceryList type, and use it to store a set of items that are already in the cart. Write a check-off function that takes a grocery list and checks off an item on it, by adding that item to the checked-off set: (check-off my-list eggs)

(defrecord GroceryList [to-buy checked-off]
  Append
  (append [this x]
    (->GroceryList (conj to-buy x) (conj checked-off #{}))))


(defn grocery-list 
  "custom constructor for a GroceryList record"
  []
  (->GroceryList [] #{}))

(defn -grocery-list
  "Creates an appendable (via IAppend) grocery list. Takes a vector of
groceries to buy."
  [to-buy]
  (reify
    Append
    (append [this x]
      (grocery-list (conj to-buy x)))

    Object
    (toString [this]

      (str "To buy: " to-buy))))

(defn check-off
  "takes a grocery list and checks off an item on it, 
  by adding that item to the checked-off set:
  (check-off my-list eggs)
  "
  [^GroceryList grocery-list item]
  (update grocery-list :checked-off conj item))

(defn remaining
  "takes a GroceryList and returns the items that haven’t been checked off yet."
  [grocery-list]
  (set/difference (set (:to-buy grocery-list)) (:checked-off grocery-list)))

(defprotocol Printable
  (print-out [x] "Print out the given object, nicely formatted."))

; Change the definition of print-out for GroceryList to take the checked-off set into account, printing an [x] in front of checked-off items.

(extend-protocol Printable
  GroceryList
  (print-out [gl]
    (println "GROCERIES")
    (println "---------")
    (doseq [item (:to-buy gl)]
      (if ((:checked-off gl) item) (print "[x] ") (print "[ ] "))
      (print-out item)
      (println)))

  Object
  (print-out [x]
    (print x)))

(defrecord CountedItem [thing quantity]
  Printable
  (print-out [this]
    (print-out thing)
    (print (str " (" quantity "x)"))))


;(-> my-list
;    (update :to-buy conj :milk)
;    (update :to-buy conj :flour))

(->GroceryList [:eggs] #{})

(def my-list (->GroceryList [:eggs :ham :beans :corn] #{:eggs :ham}))
(print-out my-list)
