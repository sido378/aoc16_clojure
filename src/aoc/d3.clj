(ns aoc.d2
  (:require [clojure.string :as str])
  (:gen-class))

(def input1 "541  588  421
827  272  126
660  514  367
 39  703  839")

(defn parse-input [input]
  (as-> input data
        (str/split data #"\n")
        (map #(str/split % #"  ") data)
        (mapv (partial mapv bigint) data)))

(defn valid? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))
        

(->> input1
     parse-input
     (filter valid?)
     count)

(seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))
