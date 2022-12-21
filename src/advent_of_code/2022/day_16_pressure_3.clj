(ns advent-of-code.2022.day-16-pressure-3
  (:require [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

;; 2022-12-20 10:27
;;    Finally got some working code that sovles part 1. I might have been too
;;    worried about pruning because the code says it exhausts the search space
;;    and I don't hit my built-in stopping points at all. I think I'll do
;;    another version of this that just does the recursive search, and not worry
;;    about keeping track of extra things in an attempt to stop early.

(comment
  (combo/combinations ["AA" "BB" "CC" "DD"] 1)
  (combo/permutations [[:A :B :C] [:D :E :F]]))

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

(comment
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))
        dists (all-dists directory)]
    (map (juxt identity dists) [["AA" "BB"] ["AA" "HH"] ["AA" "JJ"]]))
  )

(defn- flow [directory dists] ;; `init` should be [30 0]
  (let [init [30 0]]
    (fn [path]
      (->> path
           (partition 2 1)
           (reduce (fn [[minutes pressure] [a b]]
                     (let [cost (dists [a b])
                           minutes (- minutes cost 1)
                           pressure (+ pressure (* minutes (:pressure (directory b))))]
                       [minutes pressure]))
                   init)))))

(comment
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))
        dists (all-dists directory)]
    ((flow directory dists) ["AA" "BB" "HH" "CC" "EE" "JJ" "DD"]))
  )

;; TODO: what if we store only the node we are at, like ["SU"], and the valves
;; opened so far, and we only go to valves still needing to be opened, and we
;; don't replace a valve's entry in the map unless it is better than what is
;; already there? -- isn't that just dijkstra that we couldn't get to work
;; before?

(defn search [& {:keys [minutes top-n xtra directory dists skip] :or {skip #{}}}]
  (let [valves (->> directory (keep (fn [[k v]] (when (not= 0 (:pressure v)) k))) set)]
    (loop [i 0 ; a counter for pruning/stopping
           paths (pm/priority-map-keyfn-by :cost > ["AA"] {:minutes minutes :pressure 0 :cost ##Inf}) ; paths under construction
           done (pm/priority-map-keyfn-by :pressure >) ; completed paths
           bestest #{} ; top n paths
           chkpnt 0 ; when we last recorded the best n completed paths
           ]
      #_(println {:i i :paths paths :done done :best bestest :check chkpnt})
      (if (< i 1000000)
        (if-let [[path {:as info :keys [minutes pressure]}] (peek paths)]
          ;; Have a path to search.
          (let [remaining (set/difference valves (set path) skip)
                loc (peek path)
                forks (for [r remaining
                            :let [dist (dists [loc r])
                                  minutes (- minutes dist 1)]
                            :when (<= 0 minutes)
                            :let [pressure (+ pressure (* minutes (:pressure (directory r))))
                                  new-path (conj path r)
                                  remaining (set/difference valves (set new-path) skip)
                                  cost (* minutes (->> remaining (map directory) (map :pressure) (apply +)))]]
                        [new-path {:minutes minutes :pressure pressure :cost pressure #_(- pressure cost)}])]
            (if (seq forks)
              (recur (inc i) (into (pop paths) forks) done bestest chkpnt)
              (let [done (assoc done path info)
                    new-bestest (take top-n done)
                    chkpnt (if (= new-bestest bestest) chkpnt i)]
                (if (> (- i chkpnt) xtra)
                  {:state :checkpoint :i i :bestest bestest :chkpnt chkpnt}
                  (recur (inc i) (pop paths) done new-bestest chkpnt)))))
          ;; Done, no more paths to search.
          {:state :done :i i :bestest bestest :chkpnt chkpnt})
        ;; Debug limit hit.
        {:state :debug-limit :i i :path-count (count paths) :done done}))))

(comment
  ;; puzzle 1
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)]
     (search :minutes 30 :top-n 50 :xtra 50000 :directory directory :dists dists)))   ; => 2124

  ;; puzzle 2
  (let [directory (parse (slurp "input/2022/16-pressure.txt"))
        dists (all-dists directory)
        just-me (search :minutes 26 :top-n 10 :xtra 100000 :directory directory :dists dists)
        elephant (search :minutes 26 :top-n 10 :xtra 100000
                         :skip (set (->> just-me :bestest ffirst rest))
                         :directory directory  :dists dists)]
    [just-me elephant]
    )

  (+ 1608 942)
  )
