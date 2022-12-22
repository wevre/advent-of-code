(ns advent-of-code.2022.day-16-pressure
  (:require [clojure.string :as str]
            [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.math.combinatorics :as combo]
            [advent-of-code.common :as common]))

;; 2022-12-22
;;    This is not my type of problem. I really struggled. Adapted some code from
;;    @volesen to get it to work in a reasonable amount of time.

(defn parse [input]
  (->> input
       str/split-lines
       (map #(re-seq #"[A-Z]{2}|\d+" %))
       (map (fn [[f p & r]] [f {:pressure (parse-long p) :rooms (set r)}]))
       (into {})))

(defn- initial-dists [directory]
  (reduce into {} (for [k (keys directory) r (:rooms (directory k))]
                    [[[k k] 0] [[k r] 1] [[r k] 1]])))

(defn all-dists
  "Floyd-Warshall Algorithm -- finds shortest path between each pair-wise
   combination of nodes."
  [directory]
  (persistent!
   (reduce
    (fn [paths [v i j]]
      (assoc! paths [i j] (min (paths [i j] ##Inf)
                               (+ (paths [i v] ##Inf) (paths [v j] ##Inf)))))
    (transient (initial-dists directory))
    (combo/selections (keys directory) 3))))

(defn max-flow [directory dists]
  (common/z-combinator
   (fn [f loc minutes valves elephant?]
     (let [flow (get-in directory [loc :pressure])
           next-locs (for [v valves
                           :let [d (dists [loc v])]
                           :when (> minutes d)]
                       (f v (- minutes d 1) (disj valves v) elephant?))
           next-locs (cond-> next-locs
                       elephant? (conj (f "AA" 26 valves false)))]
       (+ (* flow minutes) (apply max 0 next-locs))))))

(comment
  ;; puzzle 1 -- ~1s
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)
         valves (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))) set)]
     ((max-flow directory dists) "AA" 30 valves false)))   ; => 2124

  ;; puzzle 2 -- ~28s
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)
         valves (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))) set)]
     ((max-flow directory dists) "AA" 26 valves true)))   ; => 2775
  )
