(ns advent-of-code.2020.day-23
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 23: Crab Cups ---

; Adapted from solution posted by zelark: https://github.com/zelark/AoC-2020

(set! *warn-on-reflection* true)

(defprotocol ICircleNode
  (value [this])
  (get-next [this])
  (set-next [this node])
  (insert [this node]))

(deftype CircleNode [value ^:volatile-mutable next]
  ICircleNode
  (value [_] value)
  (get-next [_] next)
  (set-next [this node] (set! next node) this)
  (insert [this node] (set-next node next) (set-next this node)))

(defn circle-node 
  "A single node that points to itself."
  [value]
  (let [node (->CircleNode value nil)]
    (set-next node node)))

(defn build-cups 
  "Returns a map of cup labels and associated (and linked) nodes."
  [labels]
  (loop [acc {} last nil [l & ls] labels]
    (if (nil? l)
      acc
      (let [node (circle-node l)]
        (when last (insert last node))
        (recur (assoc acc l node) node ls)))))

(defn next-dest [n curr]
  (let [dest (dec curr)]
    (if (< dest 1) n dest)))

(defn play [limit labels]
  (let [cups (build-cups labels)]
    (loop [i 0 curr (first labels)]
      (when (< i limit)
        (when (zero? (mod i 500000)) (println "at iteration" i))
        (let [curr-cup (cups curr)
              three (->> (iterate get-next curr-cup)
                         rest
                         (take 3))
              dest (->> curr
                        (iterate (partial next-dest (apply max labels)))
                        rest
                        (drop-while (set (map value three)))
                        first
                        cups)]
          (set-next curr-cup (get-next (last three)))
          (set-next (last three) (get-next dest))
          (set-next dest (first three))
          (recur (inc i) (value (get-next curr-cup))))))
    cups))

(defn puzzle1 [lim input]
  (let [cups (play lim (map edn/read-string (re-seq #"\d" input)))]
    (str/join (map value (take 8 (rest (iterate get-next (cups 1))))))))

(comment
  (puzzle1 100 "974618352")   ; <-- puzzle "75893264"
  (puzzle1 10 "389125467")   ; <-- example "67384529"
  )

(defn puzzle2 [lim input]
  (let [cups (play lim (into (mapv edn/read-string (re-seq #"\d" input))
                             (range 10 (inc 1e6))))]
    (reduce * (map value (take 2 (rest (iterate get-next (cups 1))))))))

(comment
  (puzzle2 1e7 "974618352")   ; <-- puzzle ""
  (puzzle2 1e7 "389125467")   ; <-- example ""
  )
