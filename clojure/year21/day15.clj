(ns year21.day15
  (:require [aoc]
            [clojure.string :as str]))


(defn parse-input [lines]
  (mapv #(aoc/line->ints % #"") lines))

(defn path-risk [risk-map]
  (let [cols (count risk-map)
        rows (count (first risk-map))]
    (reduce (fn [rmap loc]
              (update-in rmap loc #(+ % (min
                                         (get-in rmap (update loc 0 inc) 999)
                                         (get-in rmap (update loc 1 inc) 999)))))
            (assoc-in risk-map [0 0] 0) ;; starting position is not counted
            (rest
             (for [j (range (dec (count risk-map)) -1 -1)
                   i (range (dec (count (first risk-map))) -1 -1)]
               [j i])))))

(defn minimum-path-risk [lines]
  (-> (parse-input lines)
      path-risk
      (get-in [0 0])))

(defn part1 []
  (minimum-path-risk (aoc/get-input-lines 2021 15)))

(comment
  (def ex (str/split-lines "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"))

  (path-risk (parse-input ex))

  (== (minimum-path-risk ex) 40)

  (def ex2 (str/split-lines "1999999999
1999999999
1911199999
1119199999
9999199999
9999119999
9999911199
9999999199
9999999111
9999999991
"))
  (== (minimum-path-risk ex2) 20)

  )
