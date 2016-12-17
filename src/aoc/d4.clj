(ns aoc.d4
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))


(def input (line-seq (io/reader (io/resource "d4"))))
(def tinputs '("aaaaa-bbb-z-y-x-123[abxyz]"
                "a-b-c-d-e-f-g-h-987[abcde]"
                "not-a-real-room-404[oarel]"
                "totally-real-room-200[decoy]"))

(defn get_checksum [input]
  (->> input
       (re-matcher #"\[(.*)\]")
       re-find
       last))

(defn get_sectorId [input]
  (->> input
       (re-matcher #"-(\d*)\[.*\]$")
       re-find
       last))

(defn get_roomName [input]
  (as-> input data
       (re-matcher #"^(.*)-\d*\[.*\]$" data)
       (re-find data)
       (last data)))

(defn parse-room [line]
  [(get_roomName line)
   (get_sectorId line)
   (get_checksum line)]) 

(defn valid? [[roomName sectorId checksum]]
  (as-> roomName data
        (str/replace data "-" "")
        (frequencies data)
        (let [freqs data]
          (into (sorted-map-by
                  (fn [k1 k2]
                    (let [countcomp (compare (get freqs k2) (get freqs k1))]
                      countcomp
                      (if (= countcomp 0)
                          (compare k1 k2)
                          countcomp))))
            freqs))
        (take 5 data)
        (map key data)
        (reduce str data)
        (= data checksum)))
  
(defn solve [input]
  (->> input
       (map parse-room)
       (filter valid?)
       (map (fn [[roomName sectorId checksum]] (Integer/parseInt sectorId)))
       (reduce +)))

(defn solvep2 [input]
  (->> input
       (map parse-room)
       (map decrypt-room)
       (filter #(str/includes? % "northpole"))
       ffirst))
       

(defn rotate-char [offset character]
  (if (= character \-)
    " "
    (as-> character c
          (- (int c) 97)
          (+ c offset)
          (mod c 26)
          (+ c 97)
          (char c))))

(defn decrypt-room [[roomName sectorId checksum]]
  (->> roomName
       (map (partial rotate-char (Integer/parseInt sectorId)))
       (reduce str)
       (vector sectorId)))

(solve input)
(solvep2 input)


