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
        nil
    ))))

(count-islands nil)

(defn neighbours [x y]
  [[(inc x) y] [(dec x) y] [x (inc y)] [y (dec y)]])

