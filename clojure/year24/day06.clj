(ns year24.day06
  (:require [aoc]))

(defn classify-object [char]
  (case char
    \# :obstacle
    \^ :guard))

(defn objects [inp]
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
               (conj objs [(classify-object (first w)) row col]))))))

(defn parse-map [inp]
  (let [lines (aoc/lines inp)
        objs (objects inp)]
    {:rows (count lines)
     :cols (count (first lines))
     :guard {:at (subvec (first (filter (comp #{:guard} first) objs)) 1)
             :facing :up}
     :obstacles (mapv #(subvec % 1) (filter (comp #{:obstacle} first) objs))
     }))

(defn out-of-range? [rows cols [r c]]
  (or (< r 0)
      (< c 0)
      (>= r rows)
      (>= c cols)))

(defn move-guard [[r c] dir]
  (case dir
    :up [(dec r) c]
    :down [(inc r) c]
    :left [r (dec c)]
    :right [r (inc c)]))

(defn turn-guard [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up)
  )

(defn guard-path [{:keys [rows cols guard obstacles path] :as state}]
  (loop [[r c] (:at guard)
         dir (:facing guard)
         path path]
    (if-let [why (or (and (out-of-range? rows cols [r c]) :oob)
                     (and (some #{{:at [r c] :facing dir}} path) :loop))]
      (assoc state :path (reverse path) :why why)
      (let [nloc (move-guard [r c] dir)
            blocked? (some #{nloc} obstacles)
            ndir (if blocked?
                   (turn-guard dir)
                   dir)]
        (recur (move-guard [r c] ndir)
               ndir
               (conj path {:at [r c] :facing dir}))))))

(defn part1 [inp]
  (->> (parse-map inp)
       guard-path
       :path
       (map :at)
       set
       count
       ))

(comment
  (def inp "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")
  (parse-map inp)
; {:rows 10,
;  :cols 10,
;  :guard {:at [6 4], :facing :up},
;  :obstacles [[9 6] [8 0] [7 8] [6 1] [4 7] [3 2] [1 9] [0 4]]}
  (part1 inp) ; 41
  (part1 (aoc/get-input 2024 6)) ; 4819
  )

(defn find-loops [{:keys [path obstacles] :as state}]
  (loop [togo path
         been nil
         loops nil]
    (if (empty? togo)
      (assoc state :loops loops)
      (let [{:keys [at facing] :as pos} (first togo)
            npos (move-guard at facing)]
        (recur
          (rest togo)
          (conj been pos)
          (if (= :loop (:why (guard-path (assoc state 
                                                :path been
                                                :guard {:at at :facing (turn-guard facing)}
                                                :obstacles (conj obstacles npos)))))
            (conj loops npos)
            loops))))))

(defn part2 [inp]
  (->> (parse-map inp)
       guard-path
       find-loops
       :loops
       count
       ))

(comment
  (part2 inp) ; 6
  (part2 (aoc/get-input 2024 6)) ; 1972
  )


