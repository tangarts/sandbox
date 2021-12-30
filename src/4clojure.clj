;; [Problem 1: Nothing but the Truth [Elementary]](http://www.4clojure.com/problem/1)

(comment true)

;; [Problem 2: Simple Math [Elementary]](http://www.4clojure.com/problem/2)

(comment 4)

;; [Problem 3: Intro to Strings [Elementary]](http://www.4clojure.com/problem/3)

(comment "HELLO WORLD")

;; [Problem 4: Intro to Lists [Elementary]](http://www.4clojure.com/problem/4)

(comment :a :b :c)

;; [Problem 5: Lists: conj [Elementary]](http://www.4clojure.com/problem/5)

(comment '(1 2 3 4))

;; [Problem 6: Intro to Vectors [Elementary]](http://www.4clojure.com/problem/6)

(comment :a :b :c)

;; [Problem 7: Vectors: conj [Elementary]](http://www.4clojure.com/problem/7)

(comment [1 2 3 4])

;; [Problem 8: Intro to Sets [Elementary]](http://www.4clojure.com/problem/8)

(comment #{:a :b :c :d})

;; [Problem 9: Sets: conj [Elementary]](http://www.4clojure.com/problem/9)

(comment 2)

;; [Problem 10: Intro to Maps [Elementary]](http://www.4clojure.com/problem/10)

(comment 20)

;; [Problem 11: Maps: conj [Elementary]](http://www.4clojure.com/problem/11)

(comment [:b 2])

;; [Problem 12: Intro to Sequences [Elementary]](http://www.4clojure.com/problem/12)

(comment 3)

;; [Problem 13: Sequences: rest [Elementary]](http://www.4clojure.com/problem/13)

(comment [20 30 40])

;; [Problem 14: Intro to Functions [Elementary]](http://www.4clojure.com/problem/14)

(comment 8)

;; [Problem 15: Double Down [Elementary]](http://www.4clojure.com/problem/15)

(comment #(* 2 %))

;; [Problem 16: Hello World [Elementary]](http://www.4clojure.com/problem/16)

(comment #(str "Hello, " % "!"))

;; [Problem 17: Sequences: map [Elementary]](http://www.4clojure.com/problem/17)

(comment '(6 7 8))

;; [Problem 18: Sequences: filter [Elementary]](http://www.4clojure.com/problem/18)

(comment '(6 7))

;; [Problem 19: Last Element [Easy]](http://www.4clojure.com/problem/19)

(comment
#(nth % (dec (count %)))
)

;; [Problem 20: Penultimate Element [Easy]](http://www.4clojure.com/problem/20)

(comment
(comp second reverse)
(-> ["b" "c" "d"] second reverse)  
(->> ["b" "c" "d"] reverse second)
)

;; [Problem 21: Nth Element [Easy]](http://www.4clojure.com/problem/21)

(comment
(fn [coll n] (first (drop n coll)))
)

;; [Problem 22: Count a Sequence [Easy]](http://www.4clojure.com/problem/22)

(comment
#(reduce + (map (constantly 1) %))
)

;; [Problem 23: Reverse a Sequence [Easy]](http://www.4clojure.com/problem/23)

(comment
#(reduce conj () %)
)

;; [Problem 24: Sum It All Up [Easy]](http://www.4clojure.com/problem/24)

(comment
#(reduce + %)
)

;; [Problem 25: Find the odd numbers [Easy]](http://www.4clojure.com/problem/25)

(comment
#(filter odd? %)
)

;; [Problem 26: Fibonacci Sequence [Easy]](http://www.4clojure.com/problem/26)

(comment
#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
)

;; [Problem 27: Palindrome Detector [Easy]](http://www.4clojure.com/problem/27)

(comment
#(= (seq %) (reverse (seq %)))
)

;; [Problem 28: Flatten a Sequence [Easy]](http://www.4clojure.com/problem/28)

(comment
#(filter (complement sequential?) (rest (tree-seq sequential? seq %)))
)

;; [Problem 29: Get the Caps [Easy]](http://www.4clojure.com/problem/29)

(comment
#(apply str (re-seq #"[A-Z]+" %))
)

;; [Problem 30: Compress a Sequence [Easy]](http://www.4clojure.com/problem/30)

(comment
#(map first (partition-by identity %))
)

;; [Problem 31: Pack a Sequence [Easy]](http://www.4clojure.com/problem/31)

(comment
partition-by identity
)

;; [Problem 32: Duplicate a Sequence [Easy]](http://www.4clojure.com/problem/32)

(comment
mapcat #(list % %)
)

;; [Problem 33: Replicate a Sequence [Easy]](http://www.4clojure.com/problem/33)

(comment
(fn [s n] (mapcat #(repeat n %) s))
)

;; [Problem 34: Implement range [Easy]](http://www.4clojure.com/problem/34)

(comment
#(take (- %2 %1) (iterate inc %1))
)

;; [Problem 35: Local bindings [Elementary]](http://www.4clojure.com/problem/35)

(comment 7)

;; [Problem 36: Let it Be [Elementary]](http://www.4clojure.com/problem/36)

(comment [z 1 y 3 x 7])

;; [Problem 37: Regular Expressions [Elementary]](http://www.4clojure.com/problem/37)

(comment "ABC")

;; [Problem 38: Maximum value [Easy]](http://www.4clojure.com/problem/38)

(comment
#(last (sort %&))
)

;; [Problem 39: Interleave Two Seqs [Easy]](http://www.4clojure.com/problem/39)

(comment
#(mapcat vector %1 %2)
)

;; [Problem 40: Interpose a Seq [Easy]](http://www.4clojure.com/problem/40)

(comment
(fn [v coll] (butlast (mapcat #(vector % v) coll)))
)

;; [Problem 41: Drop Every Nth Item [Easy]](http://www.4clojure.com/problem/41)

(comment
#(apply concat (partition-all (dec %2) %2 %))
)

;; [Problem 42: Factorial Fun [Easy]](http://www.4clojure.com/problem/42)

(comment
#(apply * (range 1 (inc %)))
)

;; [Problem 43: Reverse Interleave [Medium]](http://www.4clojure.com/problem/43)

(comment
(fn deinterleave [coll n]
  (for [i (range n)] (take-nth n (drop i coll))))
)

;; [Problem 44: Rotate Sequence [Medium]](http://www.4clojure.com/problem/44)

(comment
(fn [n coll]
  (take (count coll) (drop (mod n (count coll)) (cycle coll))))
)

;; [Problem 45: Intro to Iterate [Easy]](http://www.4clojure.com/problem/45)

(comment [1 4 7 10 13])

;; [Problem 46: Flipping out [Medium]](http://www.4clojure.com/problem/46)

(comment
(fn [f]
  (fn [& args] (apply f (reverse args))))
)

;; [Problem 47: Contain Yourself [Easy]](http://www.4clojure.com/problem/47)

(comment 4)

;; [Problem 48: Intro to some [Easy]](http://www.4clojure.com/problem/48)

(comment 6)

;; [Problem 49: Split a sequence [Easy]](http://www.4clojure.com/problem/49)

(comment
(fn [n coll]
  [(take n coll) (drop n coll)])
)

;; [Problem 50: Split by Type [Medium]](http://www.4clojure.com/problem/50)

(comment
(comp vals (partial group-by type))
)

;; [Problem 51: Advanced Destructuring [Easy]](http://www.4clojure.com/problem/51)

(comment
(range 1 6)
)

;; [Problem 52: Intro to Destructuring [Easy]](http://www.4clojure.com/problem/52)

(comment
[2 4]
)

;; [Problem 53: Longest Increasing Sub-Seq [Hard]](http://www.4clojure.com/problem/53)

(comment
(fn longest-subseq [coll]
  (let [take-seq (fn [n pred coll]
                   (let [hits (count (take-while #(apply pred %) (partition n 1 coll)))]
                     (take (+ n hits -1) coll)))
        chop (fn [coll] (for [n (range (count coll))] (drop n coll)))
        parts (chop coll)
        seqs (map (partial take-seq 2 #(= (inc %1) %2)) parts)
        longest (apply max-key count seqs)]
    (if (< (count longest) 2)
      []
      longest)))
)

;; [Problem 54: Partition a Sequence [Medium]](http://www.4clojure.com/problem/54)

(comment
(fn p [n c] (when (and (seq c) (>= (count c) n)) (cons (take n c) (p n (drop n c)))))
)

;; [Problem 55: Count Occurrences [Medium]](http://www.4clojure.com/problem/55)

(comment
#(apply merge-with + (for [e %] {e 1}))
)

;; [Problem 56: Find Distinct Items [Medium]](http://www.4clojure.com/problem/56)

(comment
reduce #(if ((set %1) %2) %1 (conj %1 %2)) []
)

;; [Problem 57: Simple Recursion [Elementary]](http://www.4clojure.com/problem/57)

(comment
[5 4 3 2 1]
)

;; [Problem 58: Function Composition [Medium]](http://www.4clojure.com/problem/58)

(comment
(fn [& fs] (reduce (fn [f g] #(f (apply g %&))) fs))
)

;; [Problem 59: Juxtaposition [Medium]](http://www.4clojure.com/problem/59)

(comment
(fn [& fs]
  (fn [& as] (map #(apply % as) fs)))
)

;; [Problem 60: Sequence Reductions [Medium]](http://www.4clojure.com/problem/60)

(comment
(fn reduce-
  ([f coll]
    (reduce- f (first coll) (next coll)))
  ([f init [h & t :as coll]]
    (cons init
      (lazy-seq
        (if (seq coll)
          (reduce- f (f init h) t))))))
)

;; [Problem 61: Map Construction [Easy]](http://www.4clojure.com/problem/61)

(comment
#(apply hash-map (interleave %1 %2))
)

;; [Problem 62: Re-implement Iterate [Easy]](http://www.4clojure.com/problem/62)

(comment
(fn iterate- [f init]
  (cons init
    (lazy-seq
      (iterate- f (f init)))))
)

;; [Problem 63: Group a Sequence [Easy]](http://www.4clojure.com/problem/63)

(comment
#(apply merge-with into (for [v %2] {(% v) [v]}))
)

;; [Problem 64: Intro to Reduce [Elementary]](http://www.4clojure.com/problem/64)

(comment
+
)

;; [Problem 65: Black Box Testing [Medium]](http://www.4clojure.com/problem/65)

(comment
(fn [c]
  (cond
    (= (get (conj c [:t "t"]) :t) "t") :map
    (= (get (conj c :t) :t) :t) :set
    (= (first (conj (conj c :a) :b)) :b) :list
    (= (last (conj (conj c :a) :b)) :b) :vector))
)

;; [Problem 66: Greatest Common Divisor [Easy]](http://www.4clojure.com/problem/66)

(comment
(fn gcd [a b] (if (zero? b) a (recur b (mod a b))))
)

;; [Problem 67: Prime Numbers [Medium]](http://www.4clojure.com/problem/67)

(comment
(fn prime-gen [cnt]
  (let [prime? (fn [n]
                 (not (some #(and
                               (not= n %)
                               (zero? (mod n %)))
                            (range 2 (inc (Math/sqrt n))))))]
    (take cnt (filter prime? (iterate inc 2)))))
)

;; [Problem 68: Recurring Theme [Elementary]](http://www.4clojure.com/problem/68)

(comment
[7 6 5 4 3]
)

;; [Problem 69: Merge with a Function [Medium]](http://www.4clojure.com/problem/69)

(comment
(fn [f & ms]
  (reduce (fn [m1 m2]
            (reduce (fn [m [k v]]
                      (if (contains? m k)
                        (update-in m [k] f v)
                        (assoc m k v)))
                    m1 m2))
          ms))
)

;; [Problem 70: Word Sorting [Medium]](http://www.4clojure.com/problem/70)

(comment
(fn [s]
  (sort-by clojure.string/lower-case
    (re-seq #"[A-Za-z]+" s)))
)

;; [Problem 71: Rearranging Code: -> [Elementary]](http://www.4clojure.com/problem/71)

(comment
last
)

;; [Problem 72: Rearranging Code: ->> [Elementary]](http://www.4clojure.com/problem/72)

(comment
reduce +
)

;; [Problem 73: Analyze a Tic-Tac-Toe Board [Hard]](http://www.4clojure.com/problem/73)

(comment
(fn tic-tac-toe [board]
  (let [same? (fn [sec] (if (apply = sec) (first sec) nil))
        rows (map same? board)
        cols (map same? (apply map vector board))
        diag1 (same? (map get board (range 3)))
        diag2 (same? (map get board (range 2 -1 -1)))]
    (some #{:x :o} (concat rows cols [diag1] [diag2]))))
)

;; [Problem 74: Filter Perfect Squares [Medium]](http://www.4clojure.com/problem/74)

(comment
(fn [s]
  (let [nums (map #(Integer/parseInt %) (clojure.string/split s #","))
        psquare? (fn [n] (let [sqrt (Math/sqrt n)] (= (Math/floor sqrt) sqrt)))
        perfect (filter psquare? nums)]
    (apply str (interpose "," perfect))))
)

;; [Problem 75: Euler's Totient Function [Medium]](http://www.4clojure.com/problem/75)

(comment
(fn [n]
  (if (= n 1)
    1
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (count (filter #{1} (map (partial gcd n) (range 1 n)))))))
)

;; [Problem 76: Intro to Trampoline [Medium]](http://www.4clojure.com/problem/76)

(comment
(range 1 12 2)
)

;; [Problem 77: Anagram Finder [Medium]](http://www.4clojure.com/problem/77)

(comment
(fn [ws] 
  (set (map (comp set val) 
            (remove (comp #{1} count val) (group-by frequencies ws)))))
)

;; [Problem 78: Reimplement Trampoline [Medium]](http://www.4clojure.com/problem/78)

(comment
(fn [f & args] (loop [f (apply f args)] (if (fn? f) (recur (f)) f)))
)

;; [Problem 79: Triangle Minimal Path [Hard]](http://www.4clojure.com/problem/79)

(comment
(fn collapse [p] (let [combine (fn [a b] (map + (map #(apply min %) (partition 2 1 a)) b))] (first (reduce combine (reverse p)))))
)

;; [Problem 80: Perfect Numbers [Medium]](http://www.4clojure.com/problem/80)

(comment
(fn [n] 
  (= n (reduce +
    (filter 
      #(zero? (mod n %)) 
      (range 1 n)))))
)

;; [Problem 81: Set Intersection [Easy]](http://www.4clojure.com/problem/81)

(comment
(fn [a b] (set (filter #(contains? b %) a)))
)

;; [Problem 82: Word Chains [Hard]](http://www.4clojure.com/problem/82)

(comment
(fn find-chain [words]
  (let [lev (fn lev [s1 s2]
              (cond (empty? s1) (count s2)
                    (empty? s2) (count s1)
                    (= (first s1) (first s2)) (lev (rest s1) (rest s2))
                    :else (inc (min (lev (rest s1) s2)
                                    (lev s1 (rest s2))
                                    (lev (rest s1) (rest s2))))))
        neighbors (fn [words word] (filter #(= (lev word %) 1) words))
        chain (fn chain [graph visited root]
                (let [visited (conj visited root)
                      neigh (remove visited (graph root))]
                  (if (= visited words) 
                    true
                    (some (partial chain graph visited) neigh))))
        graph (into {} (for [w words] [w (neighbors words w)]))]
    (true? (some (partial chain graph #{}) words))))
)

;; [Problem 83: A Half-Truth [Easy]](http://www.4clojure.com/problem/83)

(comment
not=
)

;; [Problem 84: Transitive Closure [Hard]](http://www.4clojure.com/problem/84)

(comment
(fn [rel]
  (let [roots (into {} (for [[k v] (group-by first rel)] [k (mapv second v)]))
        children (fn children [rels e] 
                   (let [cs (get rels e [])]
                     (cons e (mapcat #(children rels %) cs))))]
    (set (mapcat #(map vector (repeat %) (rest (children roots %))) (keys roots)))))
)

;; [Problem 85: Power Set [Medium]](http://www.4clojure.com/problem/85)

(comment
(fn powerset [s]
  (reduce #(into % (for [subset %] (conj subset %2))) #{#{}} s))
)

;; [Problem 86: Happy numbers [Medium]](http://www.4clojure.com/problem/86)

(comment
(fn happy? [n]
  (letfn [(digits [n]
            (map #(Integer/parseInt (str %)) (str n)))
          (sum-of-squares [n]
            (reduce + (map #(* % %) (digits n))))]
    (boolean (some #{1} (take 100 (iterate sum-of-squares n))))))
)

;; [Problem 88: Symmetric Difference [Easy]](http://www.4clojure.com/problem/88)

(comment
#(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))
)

;; [Problem 89: Graph Tour [Hard]](http://www.4clojure.com/problem/89)

(comment
(fn eulerian [edges]
  (let [degrees (fn [edges]
                  (apply merge-with + {} (for [[a b] edges
                                               :when (not= a b)]
                                           {a 1 b 1})))
        gdeg (degrees edges)]
    (and
     (not (empty? gdeg))
     (->> (vals gdeg) (filter odd?) count (>= 2)))))
)

;; [Problem 90: Cartesian Product [Easy]](http://www.4clojure.com/problem/90)

(comment
#(set (for [a (vec %1) b (vec %2)] [a b]))
)

;; [Problem 91: Graph Connectivity [Hard]](http://www.4clojure.com/problem/91)

(comment
(fn fully-connected? [graph]
  (let [nodes (set (apply concat graph))
        full-graph (set (mapcat (fn [[a b :as n]] [n [b a]]) graph))
        children (into {} (for [[k v] (group-by first full-graph)] [k (set (map second v))]))
        connections (fn [node]
                      (->> (iterate #(into % (mapcat children %)) #{node})
                           (partition 2 1)
                           (drop-while #(apply not= %))
                           first first))]
    (every? #(= % nodes) (map connections nodes))))
)

;; [Problem 92: Read Roman numerals [Hard]](http://www.4clojure.com/problem/92)

(comment
(fn read-roman [s]
  (let [numerals {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
        nums (partition 2 1 (concat (map numerals s) [0]))]
    (reduce (fn [sum [a b]] ((if (< a b) - +) sum a)) 0 nums)))
)

;; [Problem 93: Partially Flatten a Sequence [Medium]](http://www.4clojure.com/problem/93)

(comment
(fn pflatten [tree]
  (if (every? sequential? tree)
    (mapcat pflatten tree)
    [tree]))
)

;; [Problem 94: Game of Life [Hard]](http://www.4clojure.com/problem/94)

(comment
(defn conway [board] 
  (let [cells (set (for [y (range (count board))
                         x (range (count (get board 0)))
                         :when (not= \space (get-in board [y x]))]
                     [x y]))
        width (count (first board))
        height (count board)
        neighbors (fn [[x y]]
                    (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
                      [(+ x dx) (+ y dy)]))
        step (fn [cells]
               (set (for [[loc n] (frequencies (mapcat neighbors cells))
                          :when (or (= n 3) (and (= n 2) (cells loc)))]
                      loc)))
        serialize (fn [alive] 
                    (mapv #(apply str %) 
                          (partition width
                                     (for [y (range height) x (range width) 
                                           :let [sym (if (alive [x y]) \# \space)]]
                                       sym))))]
    (-> cells step serialize)))
)


;; [Problem 95: To Tree, or not to Tree [Easy]](http://www.4clojure.com/problem/95)

(comment
(fn tree? [coll]
  (cond
    (or (seq? coll) (vector? coll))
      (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
    (nil? coll) true
    :else false))
)

;; [Problem 96: Beauty is Symmetry [Easy]](http://www.4clojure.com/problem/96)

(comment
#(= ((fn mirror [[n l r :as tree]] (when tree [n (mirror r) (mirror l)])) %) %)
)

;; [Problem 97: Pascal's Triangle [Easy]](http://www.4clojure.com/problem/97)

(comment
(fn [n]
  (last (take n (iterate #(map +' `(0 ~@%) `(~@% 0)) [1]))))
)

;; [Problem 98: Equivalence Classes [Medium]](http://www.4clojure.com/problem/98)

(comment
(fn [f coll] (into #{} (map set (vals (group-by f coll)))))
)

;; [Problem 99: Product Digits [Easy]](http://www.4clojure.com/problem/99)

(comment
(fn [a b] (mapv #(Integer/parseInt (str %)) (str (* a b))))
)

;; [Problem 100: Least Common Multiple [Easy]](http://www.4clojure.com/problem/100)

(comment
(fn [& args] 
  (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
    (/ (reduce * args) (reduce gcd args))))
)

;; [Problem 101: Levenshtein Distance [Hard]](http://www.4clojure.com/problem/101)

(comment
(memoize 
  (fn lev [s1 s2]
    (cond
      (zero? (count s1)) (count s2)
      (zero? (count s2)) (count s1)
      (= (first s1) (first s2)) (lev (rest s1) (rest s2))
      :else (inc (min (lev (rest s1) s2)
                      (lev s1 (rest s2))
                      (lev (rest s1) (rest s2)))))))
)

;; [Problem 102: intoCamelCase [Medium]](http://www.4clojure.com/problem/102)

(comment
#(let [words (clojure.string/split % #"-")] 
    (str (first words) 
         (apply str (map clojure.string/capitalize (drop 1 words)))))
)

;; [Problem 103: Generating k-combinations [Medium]](http://www.4clojure.com/problem/103)

(comment
(fn combinations [k s]
  (cond
    (zero? k) #{#{}}
    (empty? s) #{}
    :else (set (clojure.set/union 
                 (map #(conj % (first s)) (combinations (dec k) (rest s)))
                 (combinations k (rest s))))))
)

;; [Problem 104: Write Roman Numerals [Medium]](http://www.4clojure.com/problem/104)

(comment
(fn [n]
  (let [numerals {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 
                  "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
        dec->roman (fn [n] 
                     (loop [n n [[c v] & nums :as all] (reverse (sort-by val numerals)) acc []]
                       (cond
                         (zero? n) (apply str acc)
                         (> v n) (recur n nums acc)
                         :else (recur (- n v) all (conj acc c)))))]
    (dec->roman n)))
)

;; [Problem 105: Identify keys and values [Medium]](http://www.4clojure.com/problem/105)

(comment
(fn kv [acc k [v & vs]]
  (cond (nil? v) acc
        (keyword? v) (kv (assoc acc v []) v vs)
        :e (kv (update-in acc [k] conj v) k vs))) {} *
)

;; [Problem 106: Number Maze [Hard]](http://www.4clojure.com/problem/106)

(comment
(fn find-path [s e]
  (loop [opts [s] depth 1]
    (if (some #{e} opts)
      depth
      (letfn [(solutions [n]
                (concat 
                  [(* n 2) (+ n 2)]
                  (if (even? n) [(/ n 2)] [])))]
        (recur (mapcat solutions opts) (inc depth))))))
)

;; [Problem 107: Simple closures [Easy]](http://www.4clojure.com/problem/107)

(comment
(fn [n] (fn [b] (int (Math/pow b n))))
)
;; [Problem 108: Lazy Searching [Medium]](http://www.4clojure.com/problem/108)

(comment
(fn lazy-search [& colls]
  (if (= 1 (count colls))
    (first (first colls))
    (let [heads (map first colls) largest (apply max heads)]
      (if (apply = heads)
        largest
        (recur (map (fn [c] (drop-while #(< % largest) c)) colls))))))
)

;; [Problem 110: Sequence of pronunciations [Medium]](http://www.4clojure.com/problem/110)

(comment
#(rest (iterate (fn [coll] (mapcat (juxt count first) (partition-by identity coll))) %))
)

;; [Problem 111: Crossword Puzzle [Hard]](http://www.4clojure.com/problem/111)

(comment
(fn [w p]
  (let [sm (map #(replace {\space "" \_ \.} %) p)
        co (apply map list sm)
        pl (mapcat #(take-nth 2 (partition-by #{\#} %)) (concat sm co))]
    (boolean (some #(re-matches (re-pattern (apply str %)) w) pl))))
)

;; [Problem 112: Sequs Horribilis  [Medium]](http://www.4clojure.com/problem/112)

(comment
(fn [n s]
  (second 
   ((fn sequs [n s]
      (loop [cnt 0 acc [] [x & xs] s]
        (cond
         (or (nil? x) (< n cnt)) [cnt acc]
         (coll? x) (let [[c r] (sequs (- n cnt) x)
                         coll (if (empty? r) acc (conj acc r))]
                     (recur (+ c cnt) coll xs))
         :else (recur (+ x cnt) (if (< n (+ cnt x)) acc (conj acc x)) xs)))) 
    n s)))
)

;; [Problem 113: Making Data Dance [Medium]](http://www.4clojure.com/problem/113)

(comment
#(when %&
   (reify
     clojure.lang.ISeq
     (seq [_] (distinct %&))
     (toString [_] (apply str (interpose ", " (sort %&))))))
)

;; [Problem 114: Global take-while [Medium]](http://www.4clojure.com/problem/114)

(comment
(fn gtw [n f [h & t]]
  (when-not (or (zero? n) (and (= n 1) (f h)))
    (cons h (gtw (if (f h) (dec n) n) f t))))
)

;; [Problem 115: The Balance of N [Medium]](http://www.4clojure.com/problem/115)

(comment
(fn [n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))
        size (int (/ (count digits) 2))
        f (take size digits)
        l (take-last size digits)]
    (= (reduce + f) (reduce + l))))
)

;; [Problem 116: Prime Sandwich [Medium]](http://www.4clojure.com/problem/116)

(comment
(fn balanced-prime? [n]
  (let [factors (cons 2 (iterate (partial + 2) 3))
        prime? (fn [n] (not-any? #(zero? (mod n %))
                                 (take-while #(<= % (inc (Math/sqrt n))) factors)))
        prime-step (fn [n s] (first (drop-while (complement prime?) (rest (iterate (partial + s) n)))))]
    (and (> n 3)
         (prime? n)
         (= n (/ (+ (prime-step n 2) (prime-step n -2)) 2)))))
)

;; [Problem 117: For Science! [Hard]](http://www.4clojure.com/problem/117)

(comment
(fn cat-n-mouse [grid]
  (let [neighbors (fn [[x y]] [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])
        parts (for [y (range (count grid))
                    x (range (count (nth grid y)))
                    :let [e (get-in grid [y x])]]
                {({\C :cat \M :mouse \# :wall \space :space} e) [x y]})
        game (apply merge-with conj {:wall [] :space []} parts)
        spaces (conj (set (:space game)) (:mouse game))]
    (loop [open [(:cat game)] visited #{}]
      (cond
        (empty? open) false
        (= (first open) (:mouse game)) true
        :else (let [visited (conj visited (first open))
                    neigh (filter spaces (neighbors (first open)))
                    neigh (remove visited neigh)
                    open (concat (rest open) (remove visited neigh))]
                (recur open visited))))))
)

;; [Problem 118: Re-implement Map [Easy]](http://www.4clojure.com/problem/118)

(comment
(fn map- [f coll] 
  (lazy-seq 
    (when-let [s (seq coll)] 
      (cons (f (first s)) (map- f (rest s))))))
)

;; [Problem 119: Win at Tic-Tac-Toe [Hard]](http://www.4clojure.com/problem/119)

(comment
(fn ttt-win [p b]
  (let [win? (fn [board]
               (let [same? (fn [sec] (if (apply = sec) (first sec) nil))
                     rows (map same? board)
                     cols (map same? (apply map vector board))
                     diag1 (same? (map get board [0 1 2]))
                     diag2 (same? (map get board [2 1 0]))]
                 (some #{:x :o} (concat rows cols [diag1] [diag2]))))
        free (for [y (range 3) 
                   x (range 3)
                   :when (= :e (get-in b [y x]))]
               [y x])]
    (set (filter #(= p (win? (assoc-in b % p))) free))))
)

;; [Problem 120: Sum of square of digits [Easy]](http://www.4clojure.com/problem/120)

(comment
(fn sum-square [coll]
  (let [digits (fn [n] (map #(- (int %) 48) (str n)))
        square #(* % %)
        sum-digits (fn [n] (reduce + (map square (digits n))))]
    (count (filter #(< % (sum-digits %)) coll))))
)

;; [Problem 121: Universal Computation Engine [Medium]](http://www.4clojure.com/problem/121)

(comment
(fn [form]
  (fn [values]
    (let [env (merge {'+ + '- - '* * '/ /} values)]
      ((fn eval- [f]
         (if (seq? f)
           (let [[op & args] f]
             (apply (env op) (map eval- args)))
           (get env f f)))
         form))))
)

;; [Problem 122: Read a binary number [Easy]](http://www.4clojure.com/problem/122)

(comment
#(Integer/parseInt % 2)
)

;; [Problem 124: Analyze Reversi [Hard]](http://www.4clojure.com/problem/124)

(comment
(fn reversi [board p]
  (let [o '{b w w b}
        d (for [y [-1 0 1] x (if (= 0 y) [-1 1] [-1 0 1])] [y x])
        b (into {} (for [y (range 4) x (range 4)] [[y x] (get-in board [y x])]))
        e (map key (filter #(= 'e (val %)) b))
        wk (fn [st off] (take-while b (rest (iterate #(map + % off) st))))
        vl (fn [pth] (let [s (apply str (map b pth)) r (re-pattern (str (o p) "+" p))]
                       (if (re-seq r s) (take-while #(not= p (b %)) pth))))
        mv (fn [st] (set (apply concat (keep vl (map #(wk st %) d)))))]
    (into {} (for [st e :let [mvs (mv st)] :when (not-empty mvs)] [st mvs]))))
)

;; [Problem 125: Gus' Quinundrum [Hard]](http://www.4clojure.com/problem/125)

(comment
;; The most common quine
(fn [x] (str x x)) '(fn [x] (str x x))
)

;; [Problem 126: Through the Looking Class [Easy]](http://www.4clojure.com/problem/126)

(comment
Class
)

;; [Problem 128: Recognize Playing Cards [Easy]](http://www.4clojure.com/problem/128)

(comment
(fn card [[s r]] 
  (let [suits (zipmap (map str "SHCD") [:spade :heart :club :diamond])
        ranks (zipmap (map str (concat (range 2 10) "TJQKA")) (range 13))]
    {:suit (suits (str s)) :rank (ranks (str r))}))
)

;; [Problem 131: Sum Some Set Subsets [Medium]](http://www.4clojure.com/problem/131)

(comment
(fn sum-subsets [& s]
  (let [ps (fn powerset [s]
             (reduce (fn [acc e] 
                       (into acc (map conj acc (repeat e))))
                     #{#{}}
                     s))
        pss (map #(disj (ps %) #{}) s)
        sums (map (fn [p] (set (map #(apply + %) p))) pss)]
    (not (empty? (apply clojure.set/intersection sums)))))
)

;; [Problem 132: Insert between two items [Medium]](http://www.4clojure.com/problem/132)

(comment
(fn [c v s]
  (mapcat (fn [[a b]] (if (and a b (c a b)) (list a v) (list a)))
          (partition-all 2 1 s)))
)

;; [Problem 134: A nil key [Elementary]](http://www.4clojure.com/problem/134)

(comment
(fn [k m] 
  (if (contains? m k)
    (= (m k) nil)
    false))
)

;; [Problem 135: Infix Calculator [Easy]](http://www.4clojure.com/problem/135)

(comment
(fn calc [& exp]
  (reduce #(if (fn? %1) (%1 %2) (partial %2 %1)) identity exp))
)

;; [Problem 137: Digits and bases [Medium]](http://www.4clojure.com/problem/137)

(comment
(fn to-base [n b] 
  (loop [n n num ()] 
    (if (zero? n) 
      (if (empty? num) '(0) num) 
      (recur (int (/ n b)) (conj num (mod n b))))))
)

;; [Problem 141: Tricky card games [Medium]](http://www.4clojure.com/problem/141)

(comment
(fn [trump]
  (fn [hand]
    (let [by-suit (group-by :suit hand)
          win-suit (or trump (:suit (first hand)))]
      (last (sort-by :rank (win-suit by-suit))))))
)

;; [Problem 143: dot product [Easy]](http://www.4clojure.com/problem/143)

(comment
#(reduce + (map * %1 %2))
)

;; [Problem 144: Oscilrate [Medium]](http://www.4clojure.com/problem/144)

(comment
(fn [x & fs]
  (reductions #(%2 %1) x (cycle fs)))
)

;; [Problem 145: For the win [Elementary]](http://www.4clojure.com/problem/145)

(comment
(range 1 40 4)
)

;; [Problem 146: Trees into tables [Easy]](http://www.4clojure.com/problem/146)

(comment
#(into {} (for [[k v] % [k2 v2] v] [[k k2] v2]))
)

;; [Problem 147: Pascal's Trapezoid [Easy]](http://www.4clojure.com/problem/147)

(comment
(fn [row]
  (iterate #(map +' `(0 ~@%) `(~@% 0)) row))
)

;; [Problem 148: The Big Divide [Medium]](http://www.4clojure.com/problem/148)

(comment
(fn [n a b]
  (let [f #(quot (dec n) %)
        g #(/ (*' % (f %) (inc (f %))) 2)]
    (- (+ (g a) (g b)) (g (* a b)))))
)

;; [Problem 150: Palindromic Numbers [Medium]](http://www.4clojure.com/problem/150)

(comment
(let [nextp (fn [n]
              (let [p #(Long. %)
                    s (str n)
                    l (count s)
                    m (subs s 0 (Math/ceil (/ l 2)))
                    h (str (inc (p m)))
                    f (fn [s] (p (str s (subs (clojure.string/reverse s) (if (even? l) 0 1)))))
                    [a b] (map f [m h])]
                (if (>= a n) a b)))]
  #(iterate (comp nextp inc) (nextp %)))
)

;; [Problem 153: Pairwise Disjoint Sets [Easy]](http://www.4clojure.com/problem/153)

(comment
(fn [sets]
  (= (reduce + (map count sets))
     (count (reduce clojure.set/union sets))))
)

;; [Problem 156: Map Defaults [Elementary]](http://www.4clojure.com/problem/156)

(comment
#(apply hash-map (interleave %2 (repeat %1)))
)

;; [Problem 157: Indexing Sequences [Easy]](http://www.4clojure.com/problem/157)

(comment
#(map vector % (range))
)

;; [Problem 158: Decurry [Medium]](http://www.4clojure.com/problem/158)

(comment
(fn [f] (fn [& args] (reduce #(apply %1 (vector %2)) f args)))
)

;; [Problem 161: Subset and Superset [Elementary]](http://www.4clojure.com/problem/161)

(comment
#{1 2}
)

;; [Problem 162: Logical falsity and truth [Elementary]](http://www.4clojure.com/problem/162)

(comment
1
)

;; [Problem 164: Language of a DFA [Hard]](http://www.4clojure.com/problem/164)

(comment
(fn run* [dfa]
  (let [run1 (fn [[s acc]] (for [[c n] ((:transitions dfa) s)] [n (str acc c)]))
        accepted (fn [[s acc]] (when ((:accepts dfa) s) acc))]
    (mapcat #(keep accepted %) (take-while not-empty (iterate #(mapcat run1 %) (run1 [(:start dfa) ""]))))))
)

;; [Problem 166: Comparisons [Elementary]](http://www.4clojure.com/problem/166)

(comment
(fn [f a b]
  (cond 
    (f a b) :lt
    (f b a) :gt
    :else :eq))
)

;; [Problem 168: Infinite Matrix [Medium]](http://www.4clojure.com/problem/168)

(comment
(fn imat
  ([f] (imat f 0 0))
  ([f r c] (imat f r c -1 -1))
  ([f r c h w]
   (letfn [(row [st wd]
             (lazy-seq
               (when-not (zero? wd)
                 (cons st (row (inc st) (dec wd))))))]
     (map #(map (partial f %) (row c w)) (row r h)))))
)

;; [Problem 171: Intervals [Medium]](http://www.4clojure.com/problem/171)

(comment
(fn intervals [s]
  (reduce (fn [memo e]
            (let [memo (if (empty? memo) [[e e]] memo)
                  [l h] (nth memo (dec (count memo)))]
              (cond
               (= e h) memo
               (= e (inc h)) (assoc-in memo [(dec (count memo)) 1] e)
               :else (conj memo [e e])))) 
          [] (distinct (sort s))))
)

;; [Problem 173: Intro to Destructuring 2 [Easy]](http://www.4clojure.com/problem/173)

(comment
; f vs
)

;; [Problem 177: Balancing Brackets [Medium]](http://www.4clojure.com/problem/177)

(comment
(fn balanced? [s]
  (let [p {\( \) \[ \] \{ \}}
        a (set "()[]{}")]
    (empty?
      (reduce (fn [[t & b :as stack] s]
                (cond (= (p t) s) b
                      (a s) (conj stack s)
                      :else stack))
              () s))))
)

;; [Problem 178: Best Hand [Hard]](http://www.4clojure.com/problem/178)

(comment
(fn [h]
  (let [[s r] (apply map list h)
        rs (set (map frequencies (partition 5 1 "A23456789TJQKA")))
        s? (rs (frequencies r))
        f? (apply = s)
        g (frequencies (vals (frequencies r)))]
    (cond
     (and s? f?) :straight-flush
     (g 4) :four-of-a-kind
     (and (g 2) (g 3)) :full-house
     f? :flush
     s? :straight
     (g 3) :three-of-a-kind
     (= 2 (g 2)) :two-pair
     (g 2) :pair
     :else :high-card)))
)
