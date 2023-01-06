(ns advent-of-code.2022.day-19-geodes
  (:require [advent-of-code.common :as common]
            [clojure.math :as math]
            [clojure.string :as str]))

(def the-robots [:ore :clay :obsidian :geode])

(defn- blueprint [b c d e f g]
  {:ore {:ore b}
   :clay {:ore c}
   :obsidian {:ore d :clay e}
   :geode {:ore f :obsidian g}})

(defn- state<- [a b c d] (zipmap the-robots [a b c d]))

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)
       (mapv (fn [[a b c d e f g]] {:id a :blueprint (blueprint b c d e f g)}))))

(defn- time-to-build [{:keys [robots resources]} cost]
  (->> (map (apply juxt (keys cost)) [robots resources cost])
       (apply map (fn [robot onhand cost]
                    (if (pos? robot)
                      (long (math/ceil (/ (- cost onhand) robot)))
                      ##Inf)))
       (apply max 0)))

(defn- capacity [blueprint]
  (assoc (apply merge-with max (vals blueprint)) :geode ##Inf))

(defn- harvest [resources robots time]
  (merge-with + resources (update-vals robots #(* % time))))

(defn- pay [resources cost]
  (merge-with - resources cost))

(defn- builder [blueprint]
  (let [most-needed (capacity blueprint)]
    (fn [{:as state :keys [minute robots resources]}]
      (for [robot the-robots
            :when (< (-> state :robots robot) (most-needed robot))
            :let [cost (robot blueprint)
                  time (time-to-build state cost)]
            :when (< time ##Inf)]
        {:minute (+ minute (inc time))
         :robots (update robots robot inc)
         :resources (-> resources
                        (harvest robots (inc time))
                        (pay cost))}))))

(defn- geodes<- [state] (-> state :resources :geode))

(defn- hypot-geodes
  "Most geodes possible, assuming new robot every minute."
  [state stop]
  (let [time (- stop (:minute state))]
    (+ (geodes<- state)
       (* time (-> state :robots :geode))
       (if (pos? time) (/ (* time (inc time)) 2) 0))))

(defn prune [stop leader nexts]
  (->> nexts
       (filter #(<= (:minute %) stop))
       (remove #(and (= stop (:minute leader)) (< (hypot-geodes % stop) (geodes<- leader))))))

(defn- graft [stop {:keys [minute robots resources]} nexts]
  (let [time (- stop minute)]
    (if (and (empty? nexts) (pos? time))
      (list {:minute stop
             :robots robots
             :resources (-> resources (harvest robots time))})
      nexts)))

(defn- cull [blueprint state]
  ;; keep capacity, but keep more if we don't have full-capacity robots
  ;; (capacity-robot)*time or something like that.
  (let [most-needed (capacity blueprint)]
    (update state :resources (partial merge-with min) most-needed)))

(defn next-stepper [stop blueprint]
  (let [build (builder blueprint)]
    (fn [state leader]
      (->> (build state)
           (prune stop leader)
           (graft stop state)
           #_(map #(cull blueprint %))))))

(defn get-cracking [state stop blueprint]
  (let [nexts (next-stepper stop blueprint)]
    (loop [frontier [state] leader state seen #{state}]
      (if-let [state (peek frontier)]
        (let [nighs (remove seen (nexts state leader))]
          (recur (into (pop frontier) nighs)
                 (reduce (fn [l n] (if (> (geodes<- n) (geodes<- l)) n l)) leader nighs)
                 (into seen nighs)))
        leader))))

(comment
  ;; puzzle 1 -- 3.8s
  (time
   (let [blueprints (parse (slurp "input/2022/19-geodes.txt"))
         #_#_blueprint (:blueprint (second blueprints))
         stop 24
         state {:minute 0
                :robots (state<- 1 0 0 0)
                :resources (state<- 0 0 0 0)}]
     (->> blueprints
          (pmap (fn [{:keys [id blueprint]}] [id (get-cracking state stop blueprint)]))
          (map (fn [[id state]] (* id (geodes<- state))))
          (apply +))))   ; => 1487

  ;; puzzle 2 -- 30s
  (time
   (let [blueprints (take 3 (parse (slurp "input/2022/19-geodes.txt")))
         stop 32
         state {:minute 0
                :robots (state<- 1 0 0 0)
                :resources (state<- 0 0 0 0)}]
     (->> blueprints
          (pmap (fn [{:keys [blueprint]}] (get-cracking state stop blueprint)))
          (map geodes<-)
          (apply *))))   ; I'm getting 77 but that is not correct. Aargh! Duh! multiply, not add.
  )
