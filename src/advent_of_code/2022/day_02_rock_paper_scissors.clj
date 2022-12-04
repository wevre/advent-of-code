(ns advent-of-code.2022.day-02-rock-paper-scissors
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def rules {:rock {:beats :scissors :points 1}
            :paper {:beats :rock :points 2}
            :scissors {:beats :paper :points 3}})

(defn score [& {:keys [p2 outcome]}]
  (+ (get-in rules [p2 :points])
     (case outcome :lose 0 :draw 3 :win 6)))

(def map1 {"A" :rock "B" :paper "C" :scissors "X" :rock "Y" :paper "Z" :scissors})

(def map2 {"A" :rock "B" :paper "C" :scissors "X" :lose "Y" :draw "Z" :win})

(defn results1 [codes]
  (let [[p1 p2] (map map1 codes)
        outcome (cond
                  (= p1 p2) :draw
                  (= p1 (get-in rules [p2 :beats])) :win
                  :else :lose)]
    {:p1 p1 :p2 p2 :outcome outcome}))

(defn results2 [codes]
  (let [[p1 outcome] (map map2 codes)
        beats (set/map-invert (update-vals rules :beats))
        p2 (case outcome
             :lose (get-in rules [p1 :beats])
             :draw p1
             :win (beats p1))]
    {:p1 p1 :p2 p2 :outcome outcome}))

(defn puzzle [results-fn input]
  (->> input
       str/split-lines
       (map #(str/split % #"\s"))
       (map results-fn)
       (map score)
       (apply +)))

(comment
  ;; puzzle 1
  (puzzle results1 (slurp "input/2022/02-rock-paper-scissors.txt"))   ; => 15632

  ;; puzzle 2
  (puzzle results2 (slurp "input/2022/02-rock-paper-scissors.txt"))   ; => 14416
  )
