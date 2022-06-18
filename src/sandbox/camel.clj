(ns camel.core
  (:require [clojure.string :as str]))

(def cases
  {:kebab #"-"
   :snake #"_"})

(defn to-camel [line]
  (let [words (str/split line #"-")]
    (str (first words)
         (apply str (map str/capitalize (drop 1 words))))))



(def example
  (str
    '(defn example-fn []
       "the example function 'example-fn' does nothing")))

(to-camel example)
