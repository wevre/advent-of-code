(ns advent-of-code.2021.day-15
  (:require [clojure.data.priority-map :refer [priority-map]]))

;;NOTE: I tried a manhattan distance penalty tacked on to the distance, but it
;;      made no difference, so I removed it.

(def start [0 0])
(def dim 100)
(def scale 1)

(defn parse-input "Return a vector of risk values." [s]
  (->> (re-seq #"\d" s) (map #(Integer/parseInt %))))

(defn risk<-loc [coll [r c]]
  ;; A more elegant, but slower (I'm looking at you, `mod`) approach:
  ;; (nth (iterate mod-9 (nth coll [mod-r mod-c])) (+ quot-r quot-c))
  (loop [d 0 r r c c]
    (cond
      (<= dim r) (recur (inc d) (- r dim) c)
      (<= dim c) (recur (inc d) r (- c dim))
      :else (-> (nth coll (+ c (* r dim))) (+ d) dec (mod 9) inc))))

(defn neighbors [[r c]]
  (let [upp (dec (* scale dim))]
    (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]
          :let [fr (+ r dr) fc (+ c dc)]
          :when (and (<= 0 fr upp) (<= 0 fc upp))]
      [fr fc])))

(defn update-distance [risks dist]
  (fn [distances loc]
    (update distances loc (fnil min ##Inf) (+ dist (risk<-loc risks loc)))))

(defn lowest-risk [risks]
  (let [upp (dec (* scale dim))
        end [upp upp]]
    (loop [distances (priority-map start 0) visited #{}]
      (let [[node dist] (first distances)
            neighbors (->> node neighbors (remove visited))]
        (if (= node end)
          (distances node)
          (recur (-> (reduce (update-distance risks dist) distances neighbors)
                     (dissoc node))
                 (conj visited node)))))))

(comment
  (with-redefs [dim 10 scale 5]
    (time
     (->
      (parse-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
      (lowest-risk))))

  ;; puzzle 1
  (time
   (lowest-risk (parse-input (slurp "input/2021/15-chitons.txt"))))
  ;;=> 811 "Elapsed time: 2402.363662 msecs"

  ;; puzzle 2
  (time
   (with-redefs [scale 5]
     (lowest-risk (parse-input (slurp "input/2021/15-chitons.txt")))))
  ;;=> 3012 "Elapsed time: 57542.997612 msecs"
  )
