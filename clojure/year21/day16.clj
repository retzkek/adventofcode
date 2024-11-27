(ns year21.day16
  (:require [aoc]
            [clojure.string :as str]))

(defn hex->bin [msg]
  (Integer/toBinaryString (Integer/parseInt msg 16)))

(defn read-packet [bits]
  (let [version (Integer/parseInt (subs bits 0 3) 2)
        type-id (Integer/parseInt (subs bits 3 6) 2)
        [data rest-bits]
        (if (== type-id 4)
          (loop [packet []
                 packet-length 6
                 bits (drop 6 bits)]
            (let [group (take 5 bits)]
              (if (= (first group) \0)
                [(into packet (rest group))
                 (drop (- 9 (mod (+ packet-length 5) 4)) bits)]
                (recur (into packet (rest group)) (+ packet-length 5) (drop 5 bits)))))
          (let [length-type (subs bits 6 7)]
            (if (= length-type "0")
              (let [total-length (Integer/parseInt (subs bits 7 22) 2)]
                [(read-packet (take total-length (drop 22 bits)))
                 (drop (+ 22 total-length) bits)])
              (let [num-packets (Integer/parseInt (subs bits 7 18) 2)]
                [(read-packet (take total-length (drop 18 bits)))
                 (drop (+ 18 total-length) bits)]))))]
    {:version version
     :type-id type-id
     :data data
     :rest rest-bits
     }))

(defn unpack-bits [msg]
  (let [version (Integer/parseInt (subs msg 0 3) 2)
        type-id (Integer/parseInt (subs msg 3 6) 2)]
    {:version version
     :type-id type-id
     :data
     (if (== type-id 4)
       (str/join
        (reduce (fn [bits group]
                  (into bits (cond
                               (= (first group) \1) (rest group)
                               (= (count group) 4) (rest group)
                               :else [])))
                []
                (partition-all 4 (subs msg 6))))
       (let [length-type (subs msg 6 7)]
         (if (= length-type 0)
           (let [total-length (Integer/parseInt (subs msg 7 22) 2)]
             )))
       (recur ))}))


(comment
  (def ex "D2FE28")

  (-> ex
      hex->bin
      read-packet)

  )
