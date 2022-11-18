(ns retzkek.aoc21.day21
  (:require [retzkek.aoc :as aoc]
            [clojure.string :as str]))

(defn parse-input [s]
  (let [ps (str/split-lines s)]
    {:turn 0
     :last-die-roll 0
     :player 0
     :scores [0 0]
     :positions
     (mapv #(-> %
                (str/split #": ")
                (last)
                (Integer/parseInt))
           ps)}))

(def die-size 100)
(def board-size 10)
(def max-score 1000)

(defn roll-deterministic-die [last-roll]
  (inc (mod last-roll die-size)))

(defn play-deterministic [state]
  (let [d1 (roll-deterministic-die (:last-die-roll state))
        d2 (roll-deterministic-die d1)
        d3 (roll-deterministic-die d2)
        p (:player state)
        move (inc (mod (+ (nth (:positions state) p) d1 d2 d3 -1) board-size))]
    (if (>= (apply max (:scores state)) max-score)
      state
      (recur (assoc state
                    :turn (inc (:turn state))
                    :last-die-roll d3
                    :player (- 1 p)
                    :scores (update (:scores state) p #(+ % move))
                    :positions (assoc (:positions state) p move))))))

(defn score [state]
  (* (apply min (:scores state)) (:turn state) 3))

(defn part1 []
  (-> (aoc/get-input 2021 21)
      (parse-input)
      (play-deterministic)
      (score)))

(def dirac-die-outcomes
  (reduce (fn [counts n]
            (update counts n #(inc (or % 0))))
          {}
          (for [d1 (range 1 4)
                d2 (range 1 4)
                d3 (range 1 4)]
            (+ d1 d2 d3))))

(comment
  (def ex "Player 1 starting position: 4
Player 2 starting position: 8")

  (-> (parse-input ex)
      (play-deterministic)
      (score))

  )
