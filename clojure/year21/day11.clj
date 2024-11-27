(ns year21.day11
  (:require [aoc]
            [clojure.string :as str]))

(defn parse-input [lines]
  (mapv #(aoc/line->ints % #"") lines))

(defn inc-all [nss]
  (mapv (fn [row]
          (mapv #(inc %) row)) nss))

(defn find-in [nss x]
  (for [i (range (count nss))
        j (range (count (first nss)))
        :when (= x (get-in nss [i j]))]
    [i j]))

(defn reset [octs]
  (mapv (fn [row]
          (mapv #(if (> % 9) 0 %) row))
        octs))

(defn neighbors [[i j]]
  [[(dec i) (dec j)]
   [(dec i) j]
   [(dec i) (inc j)]
   [i (dec j)]
   [i (inc j)]
   [(inc i) (dec j)]
   [(inc i) j]
   [(inc i) (inc j)]])

(defn flash-neighbors [o fs]
  (reduce (fn [o fl]
            (reduce (fn [o nl]
                      (if (< (get-in o nl 10) 10)
                        (update-in o nl inc)
                        o))
                    (update-in o fl inc) ;; keep from re-flashing
                    (neighbors fl)))
          o
          fs))

(defn step [octs]
  (loop [o (inc-all octs)
         f 0]
    (let [fs (find-in o 10)
          f (+ f (count fs))]
      (if (empty? fs)
        [(reset o) f]
        (recur (flash-neighbors o fs) f)))))

(defn count-flashes [octs n-steps]
  (loop [o octs
         n n-steps
         flashes 0]
    (if (zero? n)
      flashes
      (let [[o2 f2] (step o)]
        (recur o2 (dec n) (+ flashes f2))))))

(defn part1 []
  (-> (aoc/get-input-lines 2021 11)
      parse-input
      (count-flashes 100)))

(defn all== [nss n]
  (every? true?
          (map (fn [ns]
                 (every? #(== % n) ns))
               nss)))

(defn find-synch [octs]
  (loop [o octs
         n 0]
    (if (or (all== o 0)
            (> n 999)) ;; guard
      n
      (recur (first (step o)) (inc n)))))

(defn part2 []
  (-> (aoc/get-input-lines 2021 11)
      parse-input
      find-synch))

(comment
  (def ex (str/split-lines "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

  (step (first (step (parse-input ex))))

  (find-in (parse-input ex) 5)

  (count-flashes (parse-input ex) 100)

  (find-synch (parse-input ex))

  )
