(ns retzkek.aoc21.day02
  (:require [retzkek.aoc :as aoc]))

(defn parseline
  [line]
  (when-let [m (re-matches #"(\w+) (\d+)" line)]
    [(keyword (second m)) (Integer/parseInt (nth m 2))]))

(def starting-position {:horizontal 0 :depth 0 :aim 0})

(defn move
  [position movement]
  (let [[direction distance] movement]
    (case direction
      :forward (update position :horizontal + distance)
      :down (update position :depth + distance)
      :up (update position :depth - distance)
      position)))

(defn move-all
  [position movements]
  (reduce move position movements))

(defn calc-answer
  [position]
  (* (:horizontal position) (:depth position)))

(defn part1 []
  (->> (aoc/get-input-lines 2021 2)
       (map parseline)
       (move-all starting-position)
       (calc-answer)))

(defn move2
  [position movement]
  (let [[direction x] movement]
    (case direction
      :forward (-> position
                   (update :horizontal + x)
                   (update :depth + (* (:aim position) x)))
      :down (update position :aim + x)
      :up (update position :aim - x)
      position)))

(defn part2 []
  (->> (aoc/get-input-lines 2021 2)
       (map parseline)
       (reduce move2 starting-position)
       (calc-answer)))

(comment
  (def ex ["forward 5"
           "down 5"
           "forward 8"
           "up 3"
           "down 8"
           "forward 2"]))
