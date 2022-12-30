(ns advent-of-code.2022.day-19-geodes
  (:require [advent-of-code.common :as common]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn- blueprint [b c d e f g]
  {:ore {:ore (- b)}
   :clay {:ore (- c)}
   :obsidian {:ore (- d) :clay (- e)}
   :geode {:ore (- f) :obsidian (- g)}})

(defn- info<- [ore cla obs geo] {:ore ore :clay cla :obsidian obs :geode geo})

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)
       (mapv (fn [[a b c d e f g]] {:id a :blueprint (blueprint b c d e f g)}))))

(comment
  (parse (slurp "input/2022/19-geodes.txt"))
  )

(defn- time-to-build [{:keys [robots resources]} cost]
  (->> (map (apply juxt (keys cost)) [robots resources cost])
       (apply map (fn [robot onhand cost]
                    (if (pos? robot)
                      (long (math/ceil (/ (- (+ onhand cost)) robot)))
                      ##Inf)))
       (apply max 0)))

(comment
  (let [cost {:ore -4 :clay -14}
        state {#_#_:robots (info<- 1 2 15 0)
               :resources (info<- 4 14 15 0)}]
    (time-to-build state cost)))

(defn next-states [leader cost-fn]
  (let [capacity (-> (apply merge-with min (vals cost-fn))
                     (update-vals -)
                     (assoc :geode ##Inf))
        harvest (fn [time robots & others]
                  (merge-with min (merge-with )
                              (apply merge-with + (update-vals robots #(* % time)) others)))]
    (fn [{:as state :keys [minutes robots resources]}]
      ;; One other optimization we could make, to improve memoization by
      ;; reducing alternatives, is to cap resources to the most we could used.
      ;; CONCERN: I don't have a state where I can't build any robot, and I fast
      ;; forward to minute 0 producing as much geode as I can. In fact, I'm not
      ;; sure how I'm getting to minute 0 on my output. lucky?
      (when (pos? minutes)
        (let [builds (for [robot [:geode :obsidian :clay :ore]
                           :let [time (time-to-build state (cost-fn robot))]
                           :when (and (< time minutes) (< (robots robot) (capacity robot)))]
                       {:minutes (- minutes time 1)
                        :robots (update robots robot inc)
                        :resources (harvest (inc time) robots resources (cost-fn robot))})]
          (if (seq builds)
            (remove (fn [{:keys [minutes resources]}]
                      (let [hyp-total (+ (:geode resources)
                                         (/ (* minutes (inc minutes)) 2))]
                        (< hyp-total (get-in @leader [:robots :geode] 0))))
                    builds)
            ;; I need this 'run out the clock' to get the correct solution.
            (list (-> state
                      (assoc :minutes 0)
                      (assoc :resources (harvest minutes robots resources))))))))))

(comment
  (let [state {:minutes 24 :robots (info<- 1 0 0 0) :resources (info<- 0 0 0 0)}
        cost-fn (blueprint 4 2 3 14 2 7)]
    ((next-states (atom 0) cost-fn) state))

  (let [state {:minutes 2 :robots (info<- 1 4 2 2) :resources (info<- 3 9 2 3)}
        cost-fn (blueprint 12 12 12 12 12 12) #_(blueprint 4 2 3 14 2 7)]
    ((next-states (atom {:robots {:geode 7}}) cost-fn) state)))

(comment
  (let [cost-fn (blueprint 4 2 3 14 2 7)
        most (-> (apply merge-with min (vals cost-fn))
                 (update-vals -)
                 (assoc :geode ##Inf))]
    most))

(defn- max-geodes [state-a state-b]
  (if (> (get-in state-a [:resources :geode] 0)
         (get-in state-b [:resources :geode] 0))
    state-a
    state-b))

(defn geodes [costs-fn]
  (let [leader (atom nil)
        nexts (next-states leader costs-fn)
        maxxer (fn [state-a state-b]
                 (swap! leader max-geodes state-b)
                 (max-geodes state-a state-b))]
    (common/z-combinator
     (fn [f state]
       (transduce (keep (fn [s] (when (<= 0 (:minutes s)) (f s))))
                  (completing maxxer)
                  state
                  (nexts state))))))

(comment
  (time
   (let [state {:minutes 24 :robots (info<- 1 0 0 0) :resources (info<- 0 0 0 0)}
         cost-fn (blueprint 2 3 3 8 3 12) #_(blueprint 4 2 3 14 2 7)]
     ((geodes cost-fn) state)))
  )

(comment
  ;; puzzle 1
  (time
   (let [blueprints (parse (slurp "input/2022/19-geodes.txt"))
         init-state {:minutes 24 :robots (info<- 1 0 0 0) :resources (info<- 0 0 0 0)}]
     (->> blueprints
          (pmap (fn [{:keys [id blueprint]}] [id ((geodes blueprint) init-state)]))
          (map (fn [[id {{geodes :geode} :resources}]] (* id geodes)))
          (apply +))))   ; => 1468 (too low)

  ;; puzzle 2
  (time
   (let [blueprints (parse (slurp "input/2022/19-geodes.txt"))
         init-state {:minutes 32 :robots (info<- 1 0 0 0) :resources (info<- 0 0 0 0)}]
     (->> blueprints
          (take 3)
          (map (fn [{:keys [id blueprint]}] [id ((geodes blueprint) init-state)]))
          (map (fn [[id {{geodes :geode} :resources}]] (* id geodes)))
          #_
          (apply +))))
  )
