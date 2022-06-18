(ns sandbox.algos)

(def data [1,2,3,4,5,56,78,89,100])

(defn binary-search
  [data target]
  (loop [data data
         lo 0
         hi (dec (count data))]
    (if (<= lo hi)
      (let [m (+ lo (quot (- hi lo) 2))]
        (cond
          (< (get data m) target) (recur data (inc m) hi)
          (> (get data m) target) (recur data lo (dec m))
          :else m))
      -1)))

(binary-search data 1)

(defn balanced-parens
  [string]
  (let [paren {\{ \} \( \) \[ \]}]
    (loop [stack '()
           string (seq string)]
      (cond
        (empty? string)
        (empty? stack)

        (get paren (first string))
        (recur (cons (get paren (first string)) stack) (rest string))

        (and (seq? stack) (= (first stack) (first string)))
        (recur (rest stack) (rest string))

        :else false))))

(defn count-islands
  [islands]
  (doseq [x (range (count islands))]
    (doseq [y (range (count (islands 0)))]
      (when (= (get-in islands x y) 1)
        nil))))

(count-islands nil)

(defn neighbours [x y]
  [[(inc x) y] [(dec x) y] [x (inc y)] [y (dec y)]])

(defn dfs
  [matrix [x y]]
  (loop [matrix matrix
         stack [[x y]]
         visited #{[x y]}]
    (if (empty? stack) matrix
        (let [current (peek stack)
              nb (filterv (fn [[x y]] (and (>= x 0)
                                           (>= y 0)
                                           (< x (-> matrix count))
                                           (< y (-> matrix count))
                                           (= (get-in matrix [x y]) 1)))
                          (neighbours (first current) (second current)))]
          (recur
           (assoc-in matrix current \#)
           (into (pop stack) nb)
           (into visited nb))))))

(def matrix [[1 0 0 1]
             [1 1 0 0]
             [1 1 0 1]
             [0 1 1 0]])

(dfs matrix [0 0])

(reduce (fn result [{:keys [m acc] :as v} c]
          (if (= 1 (get-in m c))
            (-> v
                (assoc :m (dfs m c))
                (update :acc inc))
            {:m m
             :acc acc}))

        {:m matrix
         :acc 0}

        (into [] (for [x (range 4) y (range 4)] [x y])))
