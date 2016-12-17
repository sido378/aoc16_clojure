(ns aoc.d1
  (:require [clojure.string :as str])
  (:gen-class))

(def input "R3, R1, R4, L4, R3, R1, R1, L3, L5, L5, L3, R1, R4, L2, L1, R3, L3, R2, R1, R1, L5, L2, L1, R2, L4, R1, L2, L4, R2, R2, L2, L4, L3, R1, R4, R3, L1, R1, L5, R4, L2, R185, L2, R4, R49, L3, L4, R5, R1, R1, L1, L1, R2, L1, L4, R4, R5, R4, L3, L5, R1, R71, L1, R1, R186, L5, L2, R5, R4, R1, L5, L2, R3, R2, R5, R5, R4, R1, R4, R2, L1, R4, L1, L4, L5, L4, R4, R5, R1, L2, L4, L1, L5, L3, L5, R2, L5, R4, L4, R3, R3, R1, R4, L1, L2, R2, L1, R4, R2, R2, R5, R2, R5, L1, R1, L4, R5, R4, R2, R4, L5, R3, R2, R5, R3, L3, L5, L4, L3, L2, L2, R3, R2, L1, L1, L5, R1, L3, R3, R4, R5, L3, L5, R1, L3, L5, L5, L2, R1, L3, L1, L3, R4, L1, R3, L2, L2, R3, R3, R4, R4, R1, L4, R1, L5")

(def input2 "R2, L3")

(def input3 "R2, R2, R2")
(def input4 "R5, L5, R5, R3")

(defn abs [x] (max x (- x)))

(defn addv [v1 v2]
  [(+ (v1 0) (v2 0))
   (+ (v1 1) (v2 1))])

(defn parse-step [step]
  [(first step) (Integer/parseInt (apply str (rest step)))])

(defn parse-input [input]
  (as-> input data
        (str/split data #", ")
        (map parse-step data)))

(defn positions [data]
 (->> 
      (map first data)
      (map (fn [x] ((get {\L dec \R inc} x) 0)))
      (reductions +)
      (map #(mod % 4))
      (map [[0 1] [1 0] [0 -1] [-1 0]])
      (map vector (map second data))
      (mapcat #(apply repeat %))
      (reductions addv [0 0])))

(defn calcCoords [steplist]
  (reductions addv [0 0] steplist))

(defn calcDistance [coords]
  (+ (abs (get coords 0)) (abs (get coords 1))))

(defn findFirstDup [positions]
  (as-> positions data
       (map-indexed (fn [idx, itm] [idx (get (frequencies positions) itm)]) data)
       (filter #(> (get % 1) 1) data)
       (first data)
       (data 0)
       (nth positions data)))

(defn solve [input]
  (->> input
       parse-input
       positions
       last
       calcDistance))

(defn solvep2 [input]
  (->> input
       parse-input
       positions
       findFirstDup
       calcDistance)) 

(solve input)
(solvep2 input)

