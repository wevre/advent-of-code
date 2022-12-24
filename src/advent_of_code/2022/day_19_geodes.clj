(ns advent-of-code.2022.day-19-geodes
  (:require [advent-of-code.common :as common]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn- build-cost [b c d e f g]
  {:ore {:ore (- b)}
   :clay {:ore (- c)}
   :obsidian {:ore (- d) :clay (- e)}
   :geode {:ore (- f) :obsidian (- g)}})

(defn- state<- [ore cla obs geo] {:ore ore :clay cla :obsidian obs :geode geo})

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)
       (mapv (fn [[a b c d e f g]] {:blueprint a :costs (build-cost b c d e f g)}))))

(comment
  (parse (slurp "input/2022/19-geodes.txt"))
  )

(defn- afford? [resources cost]
  (<= 0 (->> (merge-with + resources cost) vals (apply min))))

(comment
  (afford? {:ore 3 :clay 0 :obsidian 15 :geode 0} {:ore 3 :obsidian 14 :geode 0}))

(def $prune (atom 0))

(defn- able? [{:keys [minutes robots exclusions]}]
  (let [result
        (or (< 0 (:geode robots))
            (and (not (exclusions :geode))
                 (or (< 0 (:obsidian robots))
                     (and (not (exclusions :obsidian))
                          (or (< 0 (:clay robots))
                              (not (exclusions :clay)))))))]
    (when (< 0 minutes) (swap! $prune + minutes))
    result))

(defn next-states [cost-fn]
  (fn [{:keys [minutes robots resources exclusions]}]
    ;; don't build more robots than are needed. For example, if geode robot
    ;; needs 7 obsidian, we don't need to build more than 7 obsidian robots.
    (->>
     (for [build? [true false]
           :let [buildable (->> [:ore :clay :obsidian :geode]
                                (filter #(afford? resources (cost-fn %)))
                                (remove exclusions))]
           robot (if build? buildable [:none])
           :let [cost (cost-fn robot {:ore 0 :clay 0 :obsidian 0 :geode 0})]]
       {:minutes (dec minutes)
        :robots (cond-> robots build? (update robot inc))
        :resources (merge-with + resources robots cost)
        :exclusions (if build? #{} (into exclusions buildable))})
     #_(filter able?))))

(comment
  (let [state {:minutes 24 :exclusions #{} :robots (state<- 1 0 0 0) :resources (state<- 0 0 0 0)}
        cost-fn (build-cost 4 2 3 14 2 7)
        nexts (next-states cost-fn)]
    (->> state nexts first nexts first nexts last nexts first nexts)
    )

  )

(defn- time-to-build [{:keys [robots resources]} cost]
  (->> (map (apply juxt (keys cost)) [robots resources cost])
       (apply map (fn [robot onhand cost]
                    (if (pos? robot)
                      (long (math/ceil (/ (- (+ onhand cost)) robot)))
                      ##Inf)))
       (apply max)))

(comment
  (let [cost {:ore -4 :clay -14}
        state {:robots (state<- 1 2 15 0)
               :resources (state<- 4 14 15 0)}]
    (time-to-build state cost)))

(defn next-states' [cost-fn]
  (let [most (-> (apply merge-with min (vals cost-fn))
                 (update-vals -)
                 (assoc :geode ##Inf))]
    (fn [{:as state :keys [minutes robots resources]}]
      (for [robot [:ore :clay :obsidian :geode]
            :let [time (time-to-build state (cost-fn robot))]
            :when (and (< time minutes) (< (robots robot) (most robot)))]
        {:minutes (- minutes time 1)
         :robots (update robots robot inc)
         :resources (merge-with + resources (update-vals robots #(* % (inc time))) (cost-fn robot))}))))

(comment
  (let [state {:minutes 24 :robots (state<- 1 0 0 0) :resources (state<- 0 0 0 0)}
        cost-fn (build-cost 4 2 3 14 2 7)]
    ((next-states' cost-fn) state)))

(comment
  (let [cost-fn {:ore {:ore -4}
               :clay {:ore -2}
               :obsidian {:ore -3 :clay -14}
               :geode {:ore -2 :obsidian -7}}
        most (-> (apply merge-with min (vals cost-fn))
                 (update-vals -)
                 (assoc :geode ##Inf))]
    most))


;; Recursion is taking a long time. We can try some more pruning techniques, but
;; probably better to switch to a loop/recur with a stack (DFS) and keep track
;; of best completed path so we can prune paths that can't beat it. (Assume you
;; build a new geode robot each turn, whether or not you actually _can_ and if
;; that sum of n*(n-1)/2 is less than best so far, stop.) With a DFS we can ask
;; ourselves: what robots can I build now, or will be able to build by waiting
;; for current resources to grow and then generate each of those states (fast
;; forward the time to the point where the resources are adequate and we can
;; build that robot.)

(defn geodes [costs]
  (let [nexts (next-states costs)]
    (common/z-combinator
     (fn [f state]
       (transduce (keep (fn [s] (when (<= 0 (:minutes s)) (f s))))
                  max
                  (get-in state [:resources :geode] 0)
                  (nexts state))))))

(comment
  (time
   (do
     (reset! $prune 0)
     (let [#_#_{:keys [blueprint costs]} (nth (parse (slurp "input/2022/19-geodes.txt")) 0)
           state {:minutes 24
                  :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
                  :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
                  :exclusions #{}}
           costs {:ore {:ore -4}
                  :clay {:ore -2}
                  :obsidian {:ore -3 :clay -14}
                  :geode {:ore -2 :obsidian -7}}
           _ (println "costs" costs)
           total ((geodes costs) state)]
       (println "total geodes" total)
       (println "pruned" @$prune)
       ))
   )
  )
