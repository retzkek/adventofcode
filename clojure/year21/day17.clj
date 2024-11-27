(ns year21.day17
  (:require [aoc]
            [clojure.string :as str]))

(defn parse-input [s]
  (mapv #(Integer/parseInt %)
        (rest (re-matches #"target area: x=(-?\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)" s))))

(comment
  (def ex "target area: x=20..30, y=-10..-5")

  (parse-input ex)


  )
