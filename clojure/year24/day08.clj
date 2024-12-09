(ns year24.day08
  (:require [aoc]
            [clojure.math.combinatorics :as combo]
            [clojure.set :refer [union]]))

(def inp "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn antennae [inp]
  (loop [w inp
         [row col] [0 0]
         objs nil]
    (if (empty? w)
      objs
      (recur (rest w)
             (if (= (first w) \newline)
               [(inc row) 0]
               [row (inc col)])
             (if (#{\newline \.} (first w))
               objs
               (conj objs [(first w) row col]))))))

(defn parse-map [inp]
  (let [lines (aoc/lines inp)
        ants (antennae inp)]
    {:rows (count lines)
     :cols (count (first lines))
     :ants (group-by first ants)
     }))

(defn oob? [rows cols [r c]]
  (or (< r 0)
      (< c 0)
      (>= r rows)
      (>= c cols)))

(defn antinodes [{:keys [rows cols ants] :as state}]
  (assoc state :antinodes 
         (reduce-kv (fn [m k v]
                      (assoc m k 
                             (disj
                               (reduce (fn [l p]
                                         (let [[_ r1 c1] (first p)
                                               [_ r2 c2] (second p)
                                               dr (- r2 r1)
                                               dc (- c2 c1)
                                               a1 [(- r1 dr) (- c1 dc)]
                                               a2 [(+ r2 dr) (+ c2 dc)]]
                                           (conj l (when-not (oob? rows cols a1) a1)
                                                 (when-not (oob? rows cols a2) a2)))) 
                                       #{}
                                       (combo/combinations v 2))
                               nil)))
                    {}
                    ants)))

(defn part1 [inp]
  (->> (parse-map inp)
       antinodes
       :antinodes
       vals
       (apply union)
       count
       ))

(comment
  (part1 inp) ; 14
  (part1 (aoc/get-input 2024 8)) ; 351
  )

(defn antinodes2 [{:keys [rows cols ants] :as state}]
  (assoc state :antinodes 
         (reduce-kv (fn [m k v]
                      (assoc m k 
                             (disj
                               (reduce (fn [l p]
                                         (let [[_ r1 c1] (first p)
                                               [_ r2 c2] (second p)
                                               dr (- r2 r1)
                                               dc (- c2 c1)
                                               dd (aoc/gcd dr dc)
                                               ddr (/ dr dd)
                                               ddc (/ dc dd)]
                                           (union l 
                                                  (loop [ans #{}
                                                         r r1
                                                         c c1] 
                                                    (if (oob? rows cols [r c]) 
                                                      ans
                                                      (recur (conj ans [r c]) (- r ddr) (- c ddc))))
                                                  (loop [ans #{}
                                                         r r2
                                                         c c2] 
                                                    (if (oob? rows cols [r c]) 
                                                      ans
                                                      (recur (conj ans [r c]) (+ r ddr) (+ c ddc))))
                                                  ))) 
                                       #{}
                                       (combo/combinations v 2))
                               nil)))
                    {}
                    ants)))

(defn part2 [inp]
  (->> (parse-map inp)
       antinodes2
       :antinodes
       vals
       (apply union)
       count
       ))

(comment
  (part2 inp) ; 34
  (part2 (aoc/get-input 2024 8)) ; 1259
  )
