(ns year24.day14
  (:require [aoc]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as st]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [instaparse.core :as insta]))

(def test1 "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(def parse
  (insta/parser
    "S = bot+
    bot = <'p='> loc <' v='> loc <'\\n'>?
    loc = d <','> d
    d = #'[-\\d]+'"))

(defn parse-inp [inp]
  (->> (parse inp)
       (insta/transform
         {:d #(Integer/parseInt %)
          :loc (fn [x y] [x y])
          :bot (fn [p v] {:p p :v v})
          :S (fn [& rest] rest)})))

(defn step [{:keys [p v] :as bot} w h n]
  (assoc bot :p 
         [(mod (+ (first p) (* n (first v))) w)
          (mod (+ (second p) (* n (second v))) h)]))

(defn print-map [w h bots]
  (let [bs (reduce #(conj %1 (:p %2)) #{} bots)] 
    (doseq [x (range w)]
      (doseq [y (range h)]
        (print (if (bs [x y]) "#" " ")))
      (print "\n"))
    (print "\n"))
  bots)

(defn part2 [inp w h]
  (->> (parse-inp inp)
       (print-map w h)
       (mapv #(step % w h 1))
       (print-map w h)))


; curl http://localhost:11434/api/generate -d '{
;                  "model": "llama3.2-vision",
;                  "prompt":"Why is the sky blue?",
;                  "stream": false}'

(defn ollama [q]
  (try 
    (-> 
      (http/post "http://localhost:11434/api/generate"
                 {:body (json/encode {:model "llama3.2-vision"
                                      :prompt q
                                      :stream false})})
      :body
      (json/parse-string true)
      :response)
    (catch Exception e (do
                         (print "exception getting refresh token: " (.getMessage e))
                         (pprint (:body (ex-data e)))))))


(defn find-tree [& args]
  (loop [bots (parse-inp (aoc/get-input 2024 14))
         w 101
         h 103
         i 0]
    (let [m (with-out-str (print-map w h bots))
          p (str "Does this look like a christmas tree? Only say yes or no.\n" m)
          r (ollama p)]
      (println p)
      (println i)
      (println r)
      (when (st/index-of (st/lower-case r) "no")
        (recur (mapv #(step % w h 1) bots ) w h (inc i))))
    )
  )

(defn find-tree-m [& args]
  (loop [bots (mapv #(step % 101 103 1) (parse-inp (aoc/get-input 2024 14)))
         w 101
         h 103
         i 1]
    (let [m (with-out-str (print-map w h bots))
          p (str "Does this look like a christmas tree? Only say yes or no.\n" m)]
      (println p)
      (when (do (printf "%d: yes? " i) 
                (flush) 
                (read-line))
        (recur (mapv #(step % w h 103) bots ) w h (+ i 103))))
    )
  )
(comment
  (part2 test1 11 7)
  (step {:p [2,4] :v [2,-3]} 11 7 1) ; {:p [4 1], :v [2 -3]}
  (ollama "Is the sky blue? Only say yes or no.")  ; "Yes."
  )
