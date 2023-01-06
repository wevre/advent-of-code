(ns advent-of-code.2022.day-19-geodes
  (:require [advent-of-code.common :as common]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn- blueprint [b c d e f g]
  {:ore {:ore b}
   :clay {:ore c}
   :obsidian {:ore d :clay e}
   :geode {:ore f :obsidian g}})

(defn- state<- [a b c d] {:ore a :clay b :obsidian c :geode d})

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

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))
        state {:minute 0
               :robots (state<- 1 0 0 0)
               :resources (state<- 5 0 0 0)}]
    (map (fn [robot] [robot (time-to-build state (robot blueprint))]) [:ore :clay :obsidian :geode])))

(defn- capacity [blueprint]
  (assoc (apply merge-with max (vals blueprint)) :geode ##Inf))

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))]
    (capacity blueprint)))

(defn- harvest [resources robots time]
  (merge-with + resources (update-vals robots #(* % time))))

(comment
  (let [state {:minute 0
               :robots (state<- 1 1 0 0)
               :resources (state<- 1 2 0 0)}]
    (harvest (:resources state) (:robots state) 2)))

(defn- pay [resources cost]
  (merge-with - resources cost))

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))
        state {:minute 0
               :robots (state<- 1 0 0 0)
               :resources (state<- 5 0 0 0)}]
    (pay (:resources state) (:ore blueprint))))

(defn- build
  "Return a state in the future when `robot` is built, or nil."
  [{:as state :keys [minute robots resources]} robot blueprint]
  (let [cost (robot blueprint)
        time (time-to-build state cost)]
    (when (< time ##Inf)
      {:minute (+ minute (inc time))
       :robots (update robots robot inc)
       :resources (-> resources
                      (harvest robots (inc time))
                      (pay cost))})))

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))
        state {:minute 10
               :robots (state<- 1 3 0 0)
               :resources (state<- 4 15 0 0)}]
    (map (fn [robot] (build state robot blueprint)) [:ore :clay :obsidian :geode])))

(defn next-states
  "Return future states where we've built each (possible) robot."
  [state blueprint]
  (let [most-needed (capacity blueprint)]
    (->> [:ore :clay :obsidian :geode]
         (filter #(< (-> state :robots %) (most-needed %)))
         (keep #(build state % blueprint)))))

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))
        state {:minute 0
               :robots (state<- 1 14 0 0)
               :resources (state<- 0 0 0 0)}]
    (next-states state blueprint)))

(defn- geodes<- [state] (-> state :resources :geode))

(defn- hypot-geodes
  "Most geodes possible, assuming no resource constraints."
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
             :resources (-> resources
                            (harvest robots time))})
      nexts)))

(defn- cull [blueprint state]
  ;; keep capacity, but keep more if we don't have full-capacity robots
  ;; (capacity-robot)*time or something like that.
  (let [most-needed (capacity blueprint)]
    (update state :resources (partial merge-with min) most-needed)))

(defn nexts [state stop leader blueprint]
  (->> (next-states state blueprint)
       (prune stop leader)
       (graft stop state)
       #_
       (map #(cull blueprint %))))

(comment
  (let [blueprints (parse (slurp "input/2022/19-geodes-sample.txt"))
        blueprint (:blueprint (first blueprints))
        stop 24
        leader 0
        state {:minute 23
               :robots (state<- 4 14 1 0)
               :resources (state<- 0 14 0 0)}]
    (nexts state stop leader blueprint)))

(defn geodes [state stop blueprint]
  (loop [frontier [state] leader state seen #{state}]
    (if-let [state (peek frontier)]
      (let [nighs (remove seen (nexts state stop leader blueprint))]
        (recur (into (pop frontier) nighs)
               (reduce (fn [l n] (if (> (geodes<- n) (geodes<- l)) n l)) leader nighs)
               #_
               (transduce (map (comp :geode :resources)) max leader nighs)
               (into seen nighs)))
      leader)))

(comment
  ;; puzzle 1
  (let [blueprints (parse (slurp "input/2022/19-geodes.txt"))
        #_#_blueprint (:blueprint (second blueprints))
        stop 24
        state {:minute 0
               :robots (state<- 1 0 0 0)
               :resources (state<- 0 0 0 0)}]
    (->> blueprints
         (pmap (fn [{:keys [id blueprint]}] [id (geodes state stop blueprint)]))
         (map (fn [[id state]] (* id (geodes<- state))))
         (apply +)))   ; => 1487

  ;; puzzle 2
  (let [blueprints (take 3 (parse (slurp "input/2022/19-geodes.txt")))
        stop 32
        state {:minute 0
               :robots (state<- 1 0 0 0)
               :resources (state<- 0 0 0 0)}]
    (->> blueprints
         (pmap (fn [{:keys [blueprint]}] (geodes state stop blueprint)))
         (map geodes<-)
         (apply *)))   ; I'm getting 77 but that is not correct. Aargh! Duh! multiply, not add.
  )

;; This was my third build of the logic because I was stumped on Part 2, not
;; realizing that I needed to _multiply_ the answers, not _add_ them.

;; But anyway, I want to refactor this code because I don't like to pass around
;; `blueprint` and friends to every function. I prefer to have functions that
;; close around them and return a function to do the work. So I want to make
;; those cleanups and then I'll rest for a season.
