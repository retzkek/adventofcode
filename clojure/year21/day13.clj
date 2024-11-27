(ns year21.day13
  (:require [aoc]
            [clojure.string :as str]))

(defn parse-input [lines]
  (loop [sheet {:dots #{}
                :folds []}
         section :dots
         lines lines]
    (if (empty? lines)
      sheet
      (let [line (first lines)
            line-data (cond
                        (empty? line) nil

                        (= section :dots)
                        (zipmap [:x :y] (aoc/line->ints line #","))

                        (= section :folds)
                        (let [[_ dir loc] (re-matches #"fold along (\w)=(\d+)" line)]
                          [(keyword dir) (Integer/parseInt loc)]))]
        (recur
         (if line-data
           (update sheet section #(conj % line-data))
           sheet)
         (if line-data
           section
           :folds)
         (rest lines))))))

(defn fold-page [dots [dir loc]]
  (reduce (fn [dots dot]
            (conj dots (if (> (dir dot) loc)
                         (update dot dir #(- (* 2 loc) %))
                         dot)))
          #{}
          dots))

(defn part1 []
  (as-> (aoc/get-input-lines 2021 13) x
    (parse-input x)
    (fold-page (:dots x) (first (:folds x)))
    (count x)))

(defn fold-all [dots folds]
  (reduce (fn [dots fold]
            (fold-page dots fold))
          dots
          folds))

(defn render [dots]
  (let [[max-x max-y] (reduce (fn [maxes dot]
                                [(max (first maxes) (:x dot))
                                 (max (second maxes) (:y dot))])
                              [0 0]
                              dots)]
    (dotimes [y (inc max-y)]
      (dotimes [x (inc max-x)]
        (print (if (contains? dots {:x x :y y}) "#" ".")))
      (print \newline))))

(defn part2 []
  (as-> (aoc/get-input-lines 2021 13) x
    (parse-input x)
    (fold-all (:dots x) (:folds x))
    (render x)))

(comment
  (def ex (str/split-lines "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

  (let [inp (parse-input ex)]
    (count (fold (:dots inp) (first (:folds inp)))))

  (let [inp (parse-input ex)]
    (render (fold-all (:dots inp) (:folds inp))))

  )
