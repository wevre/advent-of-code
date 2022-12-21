(ns advent-of-code.2022.day-16-pressure-2
  (:require [clojure.string :as str]
            [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.math.combinatorics :as combo]))

;; 2022-12-16 07:05
;;    I threw something together fast this morning but it didn't get the right
;;    answer, and I had to go do other things (make breakfast, wake up kids, get
;;    them to the bus stop).
;; 2022-12-16 10:14
;;    Looking at this again I think my problem might be I'm determining the cost
;;    of opening a valve as the full cost all the way to the end of the 30
;;    minutes, instead of doing it one minute at a time. I can't implement it
;;    now, but I'll come back and see if that works.
;; 2022-12-16 11:55
;;    I assumed this was a dijkstra algorithm, shortest path kind of problem.
;;    When my first stab didn't work, then I assumed I just needed to get my
;;    definitions of the 'state' for each step, and the cost dialed in right.
;;    But that didn't get right answer either. I'm going to try one more attempt
;;    with a new idea for the cost function, and then I've got to punt and go
;;    figure something else out.
;; 2022-12-16 12:16
;;    Okay, I read through a bit of the discussion on reddit, and I think what I
;;    need to consider as my 'next move' is not the two or three adjacent nodes
;;    to where I currently am, but _every_ node (or, every un-opened node).
;; 2022-12-17 16:32
;;    Haven't had much time to work on this, and also haven't come up with a
;;    solution. Well, I did come up with something that solved the sample
;;    problem, but it is O(n!) and cannot possibly handle the input data.

(defn parse [input]
  (->> input
       str/split-lines
       (map #(re-seq #"[A-Z]{2}|\d+" %))
       (map (fn [[f p & r]] [f {:pressure (parse-long p) :rooms (set r)}]))
       (into {})))

(defn initial-dists [directory]
  (into {} (for [[i j] (combo/selections (keys directory) 2)]
             [[i j] (cond
                      (= i j) 0
                      ((get-in directory [i :rooms]) j) 1
                      :else ##Inf)])))

(defn all-dists
  "Floyd-Warshall Algorithm"
  [directory]
  (let [ks (keys directory)
        paths (transient (initial-dists directory))]
    (loop [[v & r] ks paths paths]
      (if v
        (recur r (loop [[i & r] ks paths paths]
                   (if i
                     (recur r (loop [[j & r] ks paths paths]
                                (if j
                                  (recur r (assoc! paths [i j]
                                                   (min (get paths [i j])
                                                        (+ (get paths [i v])
                                                           (get paths [v j])))))
                                  paths)))
                     paths)))
        (persistent! paths)))))

(defn flow [directory dists]
  (fn [path]
    (->> path
         (partition 2 1)
         (reduce (fn [[minutes pressure] [a b]]
                   (let [cost (dists [a b])
                         minutes (- minutes cost 1)
                         pressure (+ pressure (* minutes (:pressure (directory b))))]
                     [minutes pressure]))
                 [30 0]))))

;; TODO: Idea: Create a function that finds the 'best' path between node "AA"
;; and one of the valves. Call that function for every valve, and then choose
;; the best from those results.

(comment
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)
         valves (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))) set)

         #_#_best-sol ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]]
     (combo/count-permutations valves) #_(->> (combo/permutations valves)
                                              (map #(conj % "AA"))
                                              (map (flow directory dists))
                                              (keep (fn [[m p]] (when (< 0 m) p)))
                                              (apply max)))))

(defrecord State [loc minutes end-fn directory dists]
  dijkstra/IState
  (state-key [_this] loc)

  (cost [_this] (* -1 minutes (:pressure (directory loc))))

  (next-states [_this]
    ;; return distance (minutes burned) to each valve, including the
    ;; minute burned to open that valve.
    (for [v (->> directory (keep (fn [[k v]] (when (and (not= k loc) (pos? (:pressure v))) k))))
          :let [burned (+ 1 (dists [loc v]))]]
      (->State v (- minutes burned) end-fn directory dists)))

  (end? [_this] (end-fn _this)))

(defn solve [directory dists]
  (fn [a b]
    (->> (->State a 30 #(= (.loc %) b) directory dists)
         dijkstra/lowest-cost
         :node
         dijkstra/path
         (map #(update % ::dijkstra/state dissoc :directory :dists))
         )
    )
  )

(comment
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))
        dists (all-dists directory)
        solver (solve directory dists)
        #_#_my-sol ["AA" "JJ" "DD" "HH" "BB" "EE" "CC"]
        #_#_best-sol ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]]
    (solver "AA" "BB"))
  )

(comment
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))]
    (->> directory (keep (fn [[k v]] (when (pos? (:pressure v)) k))))))

(comment
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))
        paths (all-dists directory)
        my-sol ["AA" "JJ" "DD" "HH" "BB" "EE" "CC"]
        best-sol ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]]
    (->> best-sol
         (partition 2 1)
         (map paths))))


(comment
  (let [directory (parse (slurp "input/2022/16-pressure.txt"))
        paths (all-dists directory)
        open-valves (->> directory (keep (fn [[k v]] (when (zero? (:pressure v)) k))) set)]
    (->> (->State "AA" directory open-valves paths 30)
         dijkstra/lowest-cost
         :node
         dijkstra/path
         (map #(update % ::dijkstra/state dissoc :directory :paths))
         #_dijkstra/path))
  (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))]
    (.next-states (->State "CC" directory #{"DD"} 27 false)))

  (dissoc nil :prev)

  (parse (slurp "input/2022/16-pressure-sample.txt")))