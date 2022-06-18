(ns sandbox.stack)

; using a list as a stack
(def stack '())

; append
(conj stack 1 2 3)

; pop
(pop (-> stack (conj 1 2 3 4 5)))

; empty?
(empty? stack)
(empty? (-> stack (conj 1 2 3)))

; top element
(peek (-> stack (conj 1 2 3)))

;; Queues - use a clojure.lang.PersistentQueue/EMPTY
(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

; append 
(seq 
  (conj (queue) 1 2 3 4))
; pop

(seq 
  (pop (conj (queue) 1 2 3 4 5 6)))

;empty
(empty? (queue))
(empty? (pop (conj (queue) 1 2 3 4 5 6)))

; top element
(peek (-> (queue) (conj 1 2 3)))
