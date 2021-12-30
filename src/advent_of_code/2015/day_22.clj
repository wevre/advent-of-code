(ns advent-of-code.2015.day-22
  (:require [clojure.data.priority-map :refer [priority-map]]))

;;;; --- Day 22: Wizard Simulator 20XX ---
;;;; https://adventofcode.com/2015/day/22

(def init {:boss {:hit-points 55 :damage 8}
           :player {:hit-points 50 :mana 500 :armor 0
                    :shield () :poison () :recharge ()}})

(defn wins? [state] (<= (get-in state [:boss :hit-points]) 0))

(defn loses? [state] (<= (get-in state [:player :hit-points]) 0))

(def hard 0)

(defn apply-effects [state]
  (let [armor (or (first (get-in state [:player :shield])) 0)
        damage (or (first (get-in state [:player :poison])) 0)
        mana (or (first (get-in state [:player :recharge])) 0)]
    (-> state
        (assoc-in [:player :armor] armor)
        (update-in [:boss :hit-points] - damage)
        (update-in [:player :mana] + mana)
        (update-in [:player :shield] rest)
        (update-in [:player :poison] rest)
        (update-in [:player :recharge] rest))))

(defn boss-turn [state]
  (let [state (apply-effects state)
        damage (get-in state [:boss :damage])
        armor (get-in state [:player :armor])
        attack (- damage armor)]
    (if (or (wins? state) (loses? state))
      state
      (update-in state [:player :hit-points] - attack))))

(defn cast-missile [state]
  (-> state
      (update-in [:player :mana] - 53)
      (update-in [:boss :hit-points] - 4)))

(defn cast-drain [state]
  (-> state
      (update-in [:player :mana] - 73)
      (update-in [:boss :hit-points] - 2)
      (update-in [:player :hit-points] + 2)))

(defn cast-shield [state]
  (-> state
      (update-in [:player :mana] - 113)
      (assoc-in [:player :shield] (repeat 6 7))))

(defn cast-poison [state]
  (-> state
      (update-in [:player :mana] - 173)
      (assoc-in [:player :poison] (repeat 6 3))))

(defn cast-recharge [state]
  (-> state
      (update-in [:player :mana] - 229)
      (assoc-in [:player :recharge] (repeat 5 101))))

(defn turns [state]
  (let [state (apply-effects state)
        state (update-in state [:player :hit-points] - hard)
        mana (get-in state [:player :mana])]
    (cond
      (or (wins? state) (loses? state)) [{:turn state :cost 0}]
      (< mana 53) [{:turn (assoc-in state [:player :hit-points] 0) :cost 0}]
      :else (->> [[53  cast-missile  :missile]
                  [73  cast-drain    :drain]
                  [113 cast-shield   :shield]
                  [173 cast-poison   :poison]
                  [229 cast-recharge :recharge]]
                 (keep (fn [[c f a]] (when (<= c mana)
                                       {:turn (boss-turn (f state))
                                        :cost c
                                        :action a})))))))

(defn update-costs [node-cost]
  (fn [costs {:keys [turn cost]}]
    (update costs turn (fnil min ##Inf) (+ node-cost cost))))

(defn lowest-mana [start]
  (loop [costs (priority-map start 0) visited #{}]
    (if-let [[node cost] (peek costs)]
      (cond
        (wins? node) [node cost]
        (loses? node) (recur (pop costs) (conj visited node))
        :else (let [turns (remove #(visited (:turn %)) (turns node))]
                (recur (reduce (update-costs cost) (pop costs) turns)
                       (conj visited node))))
      :fail)))

(comment
  ;; part 1
  (lowest-mana init)   ;=> 953

  ;;part 2
  (with-redefs [hard 1]
    (lowest-mana init))   ;=> 1289
  )
