(ns sandbox.aphyr)

;; Functions

; Write a function to find out if a string is a palindrome–that is, if it looks the same forwards and backwards.
(defn is-palindrome
  "Given a string, find out if it is a palindrome–
  that is, if it looks the same forwards and backwards.
  ---
  (is-palindrome 'heleh') => true
  (is-palindrome 'hello') => false
  "
  [s]
  (= (reverse s) (seq s)))

; count number of c's in abracadabra

((frequencies "abracadabra") \c)

; write your own version of filter
(defn my-filter
  [pred coll]
  (when-let [item (first coll)]
    (if (pred item)
      (cons item (my-filter pred (rest coll)))
      (my-filter pred (rest coll)))))

;; Macros

; Using the control flow constructs we’ve learned, write a schedule function which, given an hour of the day, returns what you’ll be doing at that time. (schedule 18), for me, returns :dinner.
(defn schedule
  "given an hour of the day, return what you'll be doing at that time
  (schedule 18) => :dinner"
  [hour]
  {:pre [(pos? hour)]}
  (condp >= hour
    6 :sleep
    8 :morning_routine
    10 :code
    13 :lunch
    15 :more_code
    18 :dinner
    24 :sleep
    "hours of the day are between 0 and 24"))

(schedule 12)

; Using the threading macros, find how many numbers from 0 to 9999 are palindromes: identical when written forwards and backwards. 121 is a palindrome, as is 7447 and 5, but not 12 or 953.

(->> (range 10000) (map str) (filter is-palindrome) (count))

; Write a macro id which takes a function and a list of args: (id f a b c), and returns an expression which calls that function with the given args: (f a b c).
(defmacro id [])

; Write a macro log which uses a var, logging-enabled, to determine whether or not to print an expression to the console at compile time. If logging-enabled is false, (log :hi) should macroexpand to nil. If logging-enabled is true, (log :hi) should macroexpand to (prn :hi). Why would you want to do this check during compilation, instead of when running the program? What might you lose?

; (Advanced) Using the rationalize function, write a macro exact which rewrites any use of +, -, *, or / to force the use of ratios instead of floating-point numbers. (* 2452.45 100) returns 245244.99999999997, but (exact (* 2452.45 100)) should return 245245N

;;;;; 

;; Polymorphism

; Write a sorted function that uses cond and instance? to convert lists to sorted lists (using (sort ...)), and sets to sorted sets (using (into (sorted-set) ...)).
(defn -sorted [coll]
  (condp instance? coll
    clojure.lang.PersistentList (sort coll)

    clojure.lang.PersistentHashSet (into (sorted-set) coll)

    clojure.lang.PersistentVector (into [] (sort coll))))

(-sorted [12 3 5 6])

; Rewrite sorted as a multimethod. Using defmethod, extend sorted to handle maps.
(defmulti sorted
  "sorts a collection into the same persistent collection"
  (fn [coll] coll))

(defmethod sorted clojure.lang.PersistentHashSet
  "sorts a set and returns a sorted set"
  [coll]
  (into (sorted-set) coll))

(defmethod sorted clojure.lang.PersistentList
  "sorts a list and returns a list"
  [coll]
  (sort coll))

(defmethod sorted clojure.lang.PersistentVector
  "sorts a vector and returns a vector"
  [coll]
  (into [] (sort coll)))

(defmethod sorted :default
  "falls back to sort"
  [coll]
  (sort coll))

; Imagine Clojure had no built-in sets. Make up a Set protocol with some basic operations, like add-element, has-element?, and remove-element.

; Using a vector or list to store your elements, write a basic implementation of your Set protocol. Experiment to make sure adding the same item twice doesn’t add two copies.

; Try making larger and larger sets–say, with ten, a thousand, and a hundred thousand elements. Use (time (has-element? some-set 123)) to see how your set performance changes with size. Why is this?

; Write a different implementation of a Set, which uses a map to store its elements. Compare its performance to your list-based set.

; The deref function uses an interface called clojure.lang.IDeref to return the current value of a container type. Using deftype, define your own container type. Try @(MyContainer :hi) to verify that you can get the value of your container (:hi) back.

; [advanced] So far, we’ve worked only with immutable types. deftype lets us define mutable types by tagging a field with ^:unsynchronized-mutable, like so: (deftype DangerBox [^:unsynchronized-mutable value] ...). Design a Mutable protocol with a (write! box value) function to overwrite the value of a mutable container. Using (set! field value), build your own mutable container type which supports both Mutable and IDeref. Confirm that you can change its value using write!, and read it back using @.

; [advanced] Use your mutable container as a counter by reading its current state and writing back a value one greater–e.g. via (write! box (inc @box)). Using dotimes, perform many updates in a row, and verify that the final value of the counter is the same as the number you passed to dotimes.

; [advanced] Run this dotimes increment loop in two threads at once, using future. Is the final counter value what you expected? Why? How does this compare to using an (atom) with swap!?
