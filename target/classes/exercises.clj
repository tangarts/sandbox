(ns exercises
(:require [clojure.java.io :as io])
(:import [java.time LocalDate]))


; To grasp basics of a new language quickly, here are the exercises I use.
; Remember some programs may not good for beginners.


; (1) Display series of numbers (1,2,3,4, 5....etc) in an infinite loop. The
; program should quit if someone hits a specific key (Say ESCAPE key).

(take 10 (range))
 
; (2) Fibonacci series, swapping two variables, finding maximum/minimum among a
; list of numbers.

(defn r-fib [n]
 (cond (< n 2) 1
       :else (+ (r-fib (- n 1)) (r-fib (- n 2)))))
(map r-fib (range 10))

(defn fib-step [[a b]]
  [b (+ a b)])

(defn seq-fib [] (map last (iterate fib-step [0 1])))

(take 10 (seq-fib))

(apply max [1 2 3 4 5])
(apply min [1 2 3 4 5])
 
; (3) Accepting series of numbers, strings from keyboard and sorting them
; ascending, descending order.

(sort (repeatedly 10 #(rand-int 100))) ; ascending
(sort #(compare %2 %1) (repeatedly 10 #(rand-int 100))) ;descending
(sort #(compare %2 %1) '(\a \z \B \N \F \P \g)) ;descending
 
; (4) Reynolds number is calculated using formula (D*v*rho)/mu Where D =
; Diameter, V= velocity, rho = density mu = viscosity Write a program that will
; accept all values in appropriate units (Don't worry about unit conversion) If
; number is < 2100, display Laminar flow, If it’s between 2100 and 4000 display
; 'Transient flow' and if more than '4000', display 'Turbulent Flow' (If, else,
; then...)

(defn flow [D v rho mu]
  (let [reynold (double (/ (* D v rho) mu))]
    (cond
      (< reynold 2100) "Laminar flow"
      (< reynold 4000) "Transient flow"
      :else "Turbulent flow")))

 
; (5) Modify the above program such that it will ask for 'Do you want to
; calculate again (y/n), if you say 'y', it'll again ask the parameters. If
; 'n', it'll exit. (Do while loop)



; While running the program give value mu = 0. See what happens. Does it give
; 'DIVIDE BY ZERO' error? Does it give 'Segmentation fault..core dump?'. How to
; handle this situation. Is there something built in the language itself?
; (Exception Handling)

; (6) Scientific calculator supporting addition, subtraction, multiplication,
; division, square-root, square, cube, sin, cos, tan, Factorial, inverse,
; modulus
 
; (7) Printing output in different formats (say rounding up to 5 decimal
; places, truncating after 4 decimal places, padding zeros to the right and
; left, right and left justification)(Input output operations)

(format "%.5f" Math/PI)
(format "%.4f" Math/E)
(format "%03d" 7)
(format "%10s" "left pad")
(format "%-10s" "right pad")
 
; (8) Open a text file and convert it into HTML file. (File operations/Strings)

(.exists (io/file "example.txt"))
(with-open [file (io/writer "example.txt")]
  (doseq [i (repeatedly 10 #(rand-int 100))]
    (.write file (str i "\n"))))

(slurp "example.txt")
 
; (9) Time and Date : Get system time and convert it in different formats
; 'DD-MON-YYYY', 'mm-dd-yyyy', 'dd/mm/yy' etc.
(LocalDate/now)

; (10) Create files with date and time stamp appended to the name

(let [fmt-now (.format (java.text.SimpleDateFormat. "dd-MM-yyyy_H:m:S") (new java.util.Date))]
  (spit (str
        "my-file_"
       fmt-now)
      "hello world"))

 
; (11) Input is HTML table, Remove all tags and put data in a comma/tab
; separated file.
 
; (12) Extract uppercase words from a file, extract unique words

(let [s "This string has Upper Case and lower case words. Extract all the Upper case Words"]
  (set (re-seq #"\b[A-Z][a-z]+\b" s)))

; (13) Implement word wrapping feature (Observe how word wrap works in windows
; 'notepad')
 
; (14) Adding/removing items in the beginning, middle and end of the array.
 
; beginning

; add
(cons 0 '(1 2 3 4 5))
(conj '(1 2 3 4) 0 -1)
; remove
(pop '(1 2 3 4 5)) ; same as rest?

; middle

; end
; add
(conj [1 2 3 4] 5 6)
; remove
(pop [1 2 3 4 5])

; (15) Are these features supported by your language: Operator overloading,
; virtual functions, references, pointers etc.
