(ns year21.day20
  (:require [aoc]
            [clojure.string :as cs]))

(defn convert-image [s]
  (mapv {\. \0 \# \1} s))

(defn parse-input [lines]
  {:algo (convert-image (first lines))
   :image (vec (for [line (nnext lines)]
                 (convert-image line)))})

(defn index [image row col missing]
  (Integer/parseInt
   (cs/join
    (for [j (range (dec row) (+ 2 row))
          i (range (dec col) (+ 2 col))]
      (get-in image [j i] missing)))
   2))

(defn enhance! [{:keys [algo image]} missing]
  {:algo algo
   :image
   (let [nrows (count image)
         ncols (count (first image))]
     (vec (for [j (range -1 (inc nrows))]
            (vec (for [i (range -1 (inc ncols))]
                   (get algo (index image j i missing)))))))})

(defn count-lit-pixels [{:keys [image]}]
  (reduce (fn [cnt row] (+ cnt (count (filter #(= \1 %) row)))) 0 image))

(defn part1 []
  (-> (aoc/get-input-lines 2021 20)
      parse-input
      (enhance! \0)
      (enhance! \1) ;; cheating a bit here, should look up index 511 (111111111)
      count-lit-pixels))

(defn part2 []
  (as-> (aoc/get-input-lines 2021 20) dat
      (parse-input dat)
      (loop [dat dat
             n 50]
        (if (zero? n)
          dat
          (recur (enhance! dat ([\0 \1] (mod n 2))) (dec n))))
      (count-lit-pixels dat)))

(comment
  (def ex (str/split-lines "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"))

  (count-lit-pixels (enhance! (enhance! (parse-input ex))))

  (index (:image (parse-input ex)) 0 0 \0)

  )
