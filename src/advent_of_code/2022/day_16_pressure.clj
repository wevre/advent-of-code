(ns advent-of-code.2022.day-16-pressure
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code.common :as common]))

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

(defn max-flow [minutes directory dists]
  (common/z-combinator
   (fn g
     ([f loc valves elephant?] (g f minutes loc valves elephant?))
     ([f t loc valves elephant?]
      (let [forks (fn [v]
                    (let [t (- t (dists [loc v]) 1)]
                      (when (>= t 0)
                        (f t v (disj valves v) elephant?))))
            init (if elephant? (f minutes "AA" valves false) 0)]
        (+ (* t (get-in directory [loc :pressure]))
           (transduce (keep forks) max init valves)))))))

(comment
  ;; puzzle 1 -- ~1s (about half of that is the F-W algorithm)
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)
         valves (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))) set)]
     ((max-flow 30 directory dists) "AA" valves false)))   ; => 2124

  ;; puzzle 2 -- ~25s
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)
         valves (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))) set)]
     ((max-flow 26 directory dists) "AA" valves true)))   ; => 2775
  )

;; 2022-12-22
;;    I really struggled with this problem, and it also hit at a time when my
;;    real life was getting busy. So I took stabs at it in fits and starts and
;;    didn't make much headway. Reading a little about other's experiences I
;;    I think I got sidetracked with worrying too much about optimizing.
;;
;;    My closest solution was an over-engineered, loop/recur-based DFS that kept
;;    track of all the searched paths in a priority map and attempted to bail
;;    out when the list of completed paths didn't change for some number of
;;    iterations. It worked for part 1 but took too long for part 2.
;;
;;    So I finally took a look at other's solutions and the one I chose to
;;    follow was by @volesen, which recursively calculates (and keeps track of
;;    only) total released pressure with a DFS and reduces the alternatives with
;;    max. He also has a clever approach for part 2 that considers what the
;;    elephant could do at each step with the full 26 minutes and all the
;;    unopened valves.
;;
;;    My modifications to @volesen's original `max-flow` were: (1) I used my
;;    z-combinator to produce a memoized, recursive function, instead of
;;    `with-redefs`; (2) I used transduce with a `keep`-based xform (instead of
;;    a `for` loop with :let and :when clauses) to reduce the max pressure
;;    released. I think the transducer might be slightly faster, and it more
;;    cleanly deals with finding the max of an empty sequence.
