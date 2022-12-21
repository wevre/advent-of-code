(ns advent-of-code.2022.day-16-pressure-4
  (:require [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

;; 2022-12-20
;;    Have some working code for part 1. I can do the entire search space in
;;    about 8 seconds, but if I set some pruning parameters (by trial and error)
;;    I can get that down to about 3.5 seconds.

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

(def bailout 1000000)

(defn search [& {:keys [minutes top-n xtra directory dists]}]
  (let [valves (->> directory (keep (fn [[k v]] (when (not= 0 (:pressure v)) k))) set)
        updater (fn [m [path info]]
                  (update m path
                          (fn [entry]
                            (if (or (not entry) (> (:pressure info) (:pressure entry)))
                              info
                              entry))))]
    (loop [i 0
           space (pm/priority-map-keyfn-by :pressure > ["AA"] {:minutes minutes :pressure 0})
           done (pm/priority-map-keyfn-by :pressure >)
           bestest #{}
           checkpoint 0]
      (cond
        (>= i bailout) {:state :bailout :i i :path-count (count space) :done done}
        (empty? space) {:state :exhausted :i i :bestest (take top-n done) :check checkpoint}
        :else (let [[path {:as info :keys [minutes pressure]}] (peek space)
                    forks (for [r (set/difference valves (set path))
                                :let [dist (dists [(peek path) r])
                                      minutes (- minutes dist 1)
                                      pressure (+ pressure (* minutes (:pressure (directory r))))]
                                :when (>= minutes 0)]
                            [(conj path r) {:minutes minutes :pressure pressure}])
                    done (cond-> done (empty? forks) (assoc path info))
                    new-bestest (set (take top-n done))
                    checkpoint (if (= new-bestest bestest) checkpoint i)]
                (if (> (- i checkpoint) xtra)
                  {:state :checkpoint :i i :bestest (take top-n done) :checkpoint checkpoint}
                  (recur (inc i) (reduce updater (pop space) forks) done new-bestest checkpoint)))))))

(comment
  ;; sample puzzle -- 25msecs
  (time
   (let [directory (parse (slurp "input/2022/16-pressure-sample.txt"))
         dists (all-dists directory)]
     (search :minutes 30 :top-n 10 :xtra 500 :directory directory :dists dists)))

  ;; puzzle 1 -- ~3.5s
  (time
   (let [directory (parse (slurp "input/2022/16-pressure.txt"))
         dists (all-dists directory)]
     (search :minutes 30 :top-n 10 :xtra 50000 :directory directory :dists dists)))
  )
