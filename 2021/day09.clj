(ns retzkek.aoc21.day09
  (:require [retzkek.aoc :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [lines]
  (mapv #(aoc/line->ints % #"") lines))

(defn get-neighbors [map row col]
  [(get-in map [row (dec col)])
   (get-in map [row (inc col)])
   (get-in map [(dec row) col])
   (get-in map [(inc row) col])])

(defn low? [map row col]
  (let [height (get-in map [row col])
        neighbors (remove nil? (get-neighbors map row col))]
    (every? #(< height %) neighbors)))

(defn low-points [map]
  (for [i (range (count map))
        j (range (count (first map)))
        :when (low? map i j)]
    {:row i
     :col j
     :risk (inc (get-in map [i j]))}))

(defn part1 []
  (->> (aoc/get-input-lines 2021 9)
       parse-input
       low-points
       (reduce #(+ %1 (:risk %2)) 0)))

(def max-height 9)

(defn find-basin [hmap row col]
  (loop [loc [row col]
         stack [loc]
         points #{}]
    (if (empty? stack)
      (conj points loc)
      (let [height (get-in hmap loc)]
        (recur (peek stack)
               (into (pop stack)
                     (filter #(and (> max-height (get-in hmap % max-height) height)
                                   (not (contains? points %)))
                             [(update loc 0 dec)
                              (update loc 0 inc)
                              (update loc 1 dec)
                              (update loc 1 inc)]))
               (conj points loc))))))

(defn part2 []
  (let [hmap (parse-input (aoc/get-input-lines 2021 9))]
    (->> (low-points hmap)
         (map #(find-basin hmap (:row %) (:col %)))
         (map count)
         sort
         reverse
         (take 3)
         (reduce *))))

(comment
  (def ex (str/split "2199943210
3987894921
9856789892
8767896789
9899965678" #"\n"))

  (def hmap (parse-input ex))

  (low-points hmap)

  (get-neighbors hmap 0 0)

  (count (find-basin hmap 2 2))

  )
