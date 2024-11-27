(ns year21.day03
  (:require [aoc]))

(defn parseline
  [line]
  (map #(Integer/parseInt %) (clojure.string/split line #"")))

;; gotta be some smarter way to do this numerically...
(defn most-common-bits [ns]
  (let [cnt (count ns)
        cnts (apply map + ns)]
    (mapv #(if (>= (/ % cnt) 1/2) 1 0) cnts)))

(defn flip-bits [bits]
  (map #(- 1 %) bits))

(defn bits-to-num [bits]
  (Integer/parseInt (clojure.string/join "" bits) 2))

(defn power-consumption [bits]
  (* (bits-to-num bits)
     (bits-to-num (flip-bits bits))))

(defn part1 []
  (->> (aoc/get-input-lines 2021 3)
       (map parseline)
       most-common-bits
       power-consumption))

(defn rating
  ([ns f]
   (rating ns f 0))
  ([ns f i]
   (if (= 1 (count ns))
     (bits-to-num (first ns))
     (let [c ((most-common-bits ns) i)]
       (rating (filter #(f c (nth % i)) ns) f (inc i))))))

(defn oxygen-generator-rating
  [ns]
  (rating ns =))

(defn scrubber-rating
  [ns]
  (rating ns not=))

(defn life-support-rating
  [ns]
  (* (oxygen-generator-rating ns) (scrubber-rating ns)))

(defn part2 []
  (->> (aoc/get-input-lines 2021 3)
       (map parseline)
       life-support-rating))

(comment
  (def ex ["00100"
           "11110"
           "10110"
           "10111"
           "10101"
           "01111"
           "00111"
           "11100"
           "10000"
           "11001"
           "00010"
           "01010"])

  (apply map + (mapv #(parseline %) ex))

  (most-common-bits (mapv #(parseline %) ex))

  (bits-to-num [1 0 1 1 0])

  (bits-to-num (flip-bits [1 0 1 1 0]))

  (Integer/toBinaryString (reduce + (mapv #(Integer/parseInt % 2) ex)))

  (oxygen-generator-rating (map #(parseline %) ex))

  (scrubber-rating (map #(parseline %) ex))

  (life-support-rating (map #(parseline %) ex))

  )
