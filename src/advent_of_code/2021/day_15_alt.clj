(ns advent-of-code.2021.day-15-alt
  "Three pieces to this solution, which is an implementation of Dijkstra's
   algorithm: (1) a 1-dim vector of the input data, `risks`, and the function
   `risk<-loc` that returns the risk for a given 'location' (where a 'location'
   is a vector of `[row, col]` and basically lets us treat `risks` as if it were
   a 2-dim matrix); (2) a set of `visited` locations; and, (3) a priority-map,
   `distances`, whose keys are `[r c]` locations and whose values are tentative
   distances. Because `distances` is a priority-map, it stays sorted by
   tentative distance, so after we remove the current node from `distances` and
   add it to `visited` then the next current node is simply `(first distances)`.

   Three other implementation notes: (1) I did not load up `distances` with
   every location initialized to ##Inf, they are instead initialized on-the-fly
   by the `update-distance` function; (2) for part 2 I did not expand `risks` to
   be 25 times as big, instead the `risk<-loc` function translates larger
   coordinates back onto the original vector, and likewise adjusts the original
   risk level upward as necessary; and, (3) I switch from part 1 to part 2 by
   redefining the var `scale` using `with-redefs`."
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def start [0 0])
(def dim 100)
(def scale 1)

;;TODO: implement a heuristic that penalizes the calculated distance with the
;;      manhattan distance to the final corner. This should avoid testing nodes
;;      that are far away from the middle of the spreading wave.

(defn parse-input "Return a vector of risk values." [s]
  (->> (re-seq #"\d" s) (map #(Integer/parseInt %))))

(defn risk<-loc [coll [r c]]
  (let [wrap (fn [x d] (if (< x dim) [x d] (recur (- x dim) (inc d))))
        [nr dr] (wrap r 0)
        [nc dc] (wrap c 0)
        v (+ dr dc (nth coll (+ nc (* nr dim))))]
    (inc (mod (dec v) 9))))

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
