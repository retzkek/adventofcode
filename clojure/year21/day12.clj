(ns year21.day12
  (:require [aoc]
            [clojure.string :as str]))

(defn parse-input [lines]
  (reduce (fn [conns line]
            (let [[s e] (str/split line #"-")]
              (conj conns [s e])))
          []
          lines))

(defn nodes-from [conns s]
  (into
   (map second (filter #(= (first %) s) conns))
   (map first (filter #(= (second %) s) conns))))

(defn small-cave? [s]
  (== (Character/getType (first s)) 2))

(defn paths-between [conns start end & [visited]]
  (if (= start end)
    (conj visited end)
    (let [visited (or visited [])
          next-nodes (remove (fn [x]
                               (and (small-cave? x)
                                    (some #(= x %) visited)))
                             (nodes-from conns start))]
      (if (empty? next-nodes)
        []
        (reduce
         (fn [paths node]
           (into paths
                 (paths-between conns node end (conj visited start))))
         (if (some #(= end %) next-nodes)
           [(conj visited start end)]
           [])
         (remove #(= end %) next-nodes))))))

(defn part1 []
  (-> (aoc/get-input-lines 2021 12)
      parse-input
      (paths-between "start" "end")
      count))

(comment
  (def ex (str/split-lines "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))

  (def conns (parse-input ex))

  (nodes-from conns "start")

  (count (paths-between conns "start" "end"))


  (def conns (parse-input (str/split-lines "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")))

  )
