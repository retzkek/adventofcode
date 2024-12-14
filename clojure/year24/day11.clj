(ns year24.day11
  (:require [aoc]
            [clojure.string :refer [join]]))

(def test1 "0 1 10 99 999")
(def test2 "125 17")

(defn tr-rock [n]
  (if (= n 0)
    1
    (let [s (str n)
          c (count s)]
      (if (= 0 (rem c 2)) 
        (mapv #(Integer/parseInt (join "" %)) (split-at (/ c 2) s))
        (* n 2024)))))

(defn tr-rocks [ns]
  (flatten (map tr-rock ns)))

(defn part1 [inp]
  (->> (aoc/line->ints inp #"[ \n]+")
       (iterate tr-rocks)
       (take 26)
       last
       count))

(comment
  (part1 test1)
  (part1 test2) ; 55312
  (part1 (aoc/get-input 2024 11)) ; 212655
  )
