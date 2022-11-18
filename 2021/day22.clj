(ns retzkek.aoc21.day22
  (:require [retzkek.aoc :as aoc]
            [clojure.set :as cs]
            [clojure.string :as str]))

(def cuboid-keys [:x0 :x1 :y0 :y1 :z0 :z1])

(defn cuboid [ranges]
  (zipmap cuboid-keys ranges))

(defn parse-step [line]
  (let [[_ action & ranges]
        (re-matches #"(\w+) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" line)]
    (assoc (cuboid (map #(Integer/parseInt %) ranges)) :action action)))

(defn congruent? [a b]
  (= (select-keys a cuboid-keys)
     (select-keys b cuboid-keys)))

(defn encloses? [a b]
  "does cuboid a fully contain cuboid b?"
  (and (<= (:x0 a) (:x0 b))
       (>= (:x1 a) (:x1 b))
       (<= (:y0 a) (:y0 b))
       (>= (:y1 a) (:y1 b))
       (<= (:z0 a) (:z0 b))
       (>= (:z1 a) (:z1 b))))

(defn intersects? [a b]
  "does cuboid a intersect cuboid b?"
  (and (or (<= (:x0 a) (:x0 b) (:x1 a))
           (<= (:x0 a) (:x1 b) (:x1 a))
           (<= (:x0 b) (:x0 a) (:x1 a) (:x1 b)))
       (or (<= (:y0 a) (:y0 b) (:y1 a))
           (<= (:y0 a) (:y1 b) (:y1 a))
           (<= (:y0 b) (:y0 a) (:y1 a) (:y1 b)))
       (or (<= (:z0 a) (:z0 b) (:z1 a))
           (<= (:z0 a) (:z1 b) (:z1 a))
           (<= (:z0 b) (:z0 a) (:z1 a) (:z1 b)))))

(defn volume [c]
  (* (- (:x1 c) (:x0 c) -1)
     (- (:y1 c) (:y0 c) -1)
     (- (:z1 c) (:z0 c) -1)))

(defn subtract [a b]
  "split cuboid a into 26 or fewer cuboids that result from subtracting cuboid b"
  (for [xr [[(:x0 a) (dec (:x0 b))] [(:x0 b) (:x1 b)] [(inc (:x1 b)) (:x1 a)]]
        yr [[(:y0 a) (dec (:y0 b))] [(:y0 b) (:y1 b)] [(inc (:y1 b)) (:y1 a)]]
        zr [[(:z0 a) (dec (:z0 b))] [(:z0 b) (:z1 b)] [(inc (:z1 b)) (:z1 a)]]
        :let [c (cuboid [(first xr) (second xr) (first yr) (second yr) (first zr) (second zr)])]
        :when (not (or (= c b)
                       (<= (volume c) 0)))]
    c))

(defn join [a b]
  "return the fifteen or fewer cuboids that are the union minus the intersection
  of cuboids a and b"
  [(cuboid [(:x0 a) (:x0)])]
  )

(defn run-step
  ([] [])
  ([step] (run-step [] step))
  ([state step]
   (into
    (if (= (:action step) "on")
      (dissoc step :action)
      [])
    (reduce
     (fn [state' c]
       (cond
         (encloses? step c)
         state'

         (encloses? c step)
         (into state' (subtract c step))

         (intersects? c step)
         (into state' (join c step))
         )
   (let [coords (set (ranges->coords (:xrange step) (:yrange step) (:zrange step)))]
     (if (= (:action step) "on")
       (cs/union reactor coords)
       (cs/difference reactor coords)))))

(defn startup-reactor [steps]
  (->> steps
       (map parse-step)
       (reduce run-step #{})
       (count)))

(defn part1 []
  (startup-reactor (aoc/get-input-lines 2021 22)))

(comment
  (parse-step "on x=1..2,y=3..4,z=5..6")

  (run-step (parse-step "on x=1..2,y=3..4,z=5..6"))

  (def ex (str/split-lines "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"))

  (startup-reactor ex)

  (startup-reactor (str/split-lines "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"))

  (intersects? {:x0 0 :x1 3 :y0 0 :y1 3 :z0 0 :z1 3}
               {:x0 2 :x1 5 :y0 4 :y1 5 :z0 0 :z1 3})

  (volume {:x0 1 :x1 3 :y0 1 :y1 3 :z0 1 :z1 3})

  )
