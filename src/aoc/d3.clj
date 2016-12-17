(ns aoc.d3
  (:require [clojure.java.io :as io])
  (:gen-class))


(def input (line-seq (io/reader (io/resource "d3"))))

(def input1 "541  588  421
827  272  126
660  514  367
 39  703  839")

(defn parse-input [line]
  (read-string (str "[" line "]")))

        

(defn valid? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))
        
(defn solve [input]
  (->> input
       (map parse-input)
       (filter valid?)
       count))

(defn solvep2 [input]
  (->> input
       (map parse-input)
       (partition 3)
       (mapcat (partial apply mapv vector))
       (filter valid?)
       count))
(solve input)
(solvep2 input)

