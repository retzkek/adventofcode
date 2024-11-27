(ns year21.day05
  (:require [aoc]
            [clojure.string :as str]))

(def line-pattern #"(\d+),(\d+) -> (\d+),(\d+)")

(defn parseline
  [line]
  (when-let [m (re-matches line-pattern line)]
    (mapv #(Integer/parseInt %) (rest m))))

(defn horiz-or-vert? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn segment->points [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (map #(vector x1 %) (if (> y2 y1)
                                    (range y1 (inc y2))
                                    (range y1 (dec y2) -1)))
    (= y1 y2) (map #(vector % y1) (if (> x2 x1)
                                    (range x1 (inc x2))
                                    (range x1 (dec x2) -1)))
    :else (map #(vector %1 %2)
               (if (> x2 x1)
                 (range x1 (inc x2))
                 (range x1 (dec x2) -1))
               (if (> y2 y1)
                 (range y1 (inc y2))
                 (range y1 (dec y2) -1)))))

(defn draw-lines [segments]
  (reduce (fn [canvas seg]
               (reduce (fn [canvas pt]
                         (update canvas pt #(inc (or % 0))))
                       canvas
                       (segment->points seg)))
          {}
          segments))

(defn count-overlaps [canvas]
  (count (filter #(> % 1) (map (fn [[k v]] v) canvas))))

(defn part1 []
  (->> (aoc/get-input-lines 2021 5)
       (map parseline)
       (filter horiz-or-vert?)
       draw-lines
       count-overlaps))

(defn part2 []
  (->> (aoc/get-input-lines 2021 5)
       (map parseline)
       draw-lines
       count-overlaps))


(comment
  (def ex (str/split "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2" #"\n"))

  (filter horiz-or-vert? (map parseline ex))

  (segment->points [3 4 1 4])

  (segment->points [2 2 2 1])

  (count-overlaps (draw-lines (filter horiz-or-vert? (map parseline ex))))

  (segment->points [1 1 4 4])

  (segment->points [4 4 1 1])

  (segment->points [4 1 1 4])

  (count-overlaps (draw-lines (map parseline ex)))
  )
