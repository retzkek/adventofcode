(ns year23.day03
  (:require [aoc]
            [clojure.string :as str]))

(defn walk-map
  [map & {:keys [from to hist] :or {from [0 0] to :right}}]
  (let [cell (get-in map from)
        x (first from)
        y (second from)]
    (cond 
      (nil? cell) hist
      (or (= cell ".") 
          (and (or (= to :left) (= to :right))
               (= cell "-"))
          (and (or (= to :up) (= to :down))
               (= cell "|")))
      (walk-map map 
                :from (case to
                        :left [(dec x) y]
                        :right [(inc x) y]
                        :up [x (dec y)]
                        :down [x (inc y)])
                :to to
                :hist (cons from hist))
      (= cell "|")
      (cons (list 
              (walk-map map
                        :from [x (dec y)]
                        :to :up
                        :hist (cons from hist))
              (walk-map map
                        :from [x (inc y)]
                        :to :down
                        :hist (cons from hist)))
            hist)
      (= cell "-")
      (cons (list 
              (walk-map map
                        :from [(dec x) y]
                        :to :left
                        :hist (cons from hist))
              (walk-map map
                        :from [(inc x) y]
                        :to :right
                        :hist (cons from hist)))
            hist)
      (= cell "\\") hist
      (= cell "/") hist
      :else (throw (Exception. "unhandled case")))))


(comment
  (def testdata ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

  (->> (str/split testdata #"\n")
       (mapv #(str/split % #""))
       (walk-map)
       )
  (walk-map [["." "."] ["." "."]] :from [0 0] :to :right)
  (aoc/get-input-map 2023 16)
  (get-in (aoc/get-input-map 2023 16) [0 1])
  )
