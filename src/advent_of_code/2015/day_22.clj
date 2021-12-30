(ns advent-of-code.2015.day-22
  (:require [clojure.data.priority-map :refer [priority-map]]))

;;;; --- Day 22: Wizard Simulator 20XX ---
;;;; https://adventofcode.com/2015/day/22

;;; Initial state

(def init {:boss {:hit-points 55 :damage 8}
           :player {:hit-points 50 :mana 500 :armor 0
                    :shield () :poison () :recharge ()}})

(defn wins? [state] (<= (get-in state [:boss :hit-points]) 0))

(defn loses? [state] (<= (get-in state [:player :hit-points]) 0))

(def hard 0)

;;; Spells and effects

(defn apply-effect [state [queue keys f]]
  (let [amt (or (first (get-in state [:player queue])) 0)]
    (-> state
        (update-in keys f amt)
        (update-in [:player queue] rest))))

(defn apply-effects [state]
  (reduce apply-effect
          state
          [[:shield   [:player :armor]    (fn [_old new] new)]
           [:poison   [:boss :hit-points] -]
           [:recharge [:player :mana]     +]]))

(defn cast-hit-point-spell [state mana boss player]
  (-> state
      (update-in [:player :mana] - mana)
      (update-in [:boss :hit-points] - boss)
      (update-in [:player :hit-points] + player)))


(defn cast-lingering-spell [state mana kind cnt amt]
  (-> state
      (update-in [:player :mana] - mana)
      (assoc-in [:player kind] (repeat cnt amt))))

(def spell-book [[:missile  cast-hit-point-spell 53 4 0]
                 [:drain    cast-hit-point-spell 73 2 2]
                 [:shield   cast-lingering-spell 113 :shield 6 7]
                 [:poison   cast-lingering-spell 173 :poison 6 3]
                 [:recharge cast-lingering-spell 229 :recharge 5 101]])

;;; Possible turns

(defn boss-turn [state]
  (let [state (apply-effects state)
        damage (get-in state [:boss :damage])
        armor (get-in state [:player :armor])
        attack (- damage armor)]
    (if (or (wins? state) (loses? state))
      state
      (update-in state [:player :hit-points] - attack))))

(defn turns [state]
  (let [state (-> state apply-effects (update-in [:player :hit-points] - hard))
        mana-avail (get-in state [:player :mana])]
    (cond
      (or (wins? state) (loses? state)) [{:turn state :cost 0}]
      (< mana-avail 53) [{:turn (assoc-in state [:player :hit-points] 0) :cost 0}]
      :else (keep (fn [[action spell mana & args]]
                         (when (<= mana mana-avail)
                           {:turn (boss-turn (apply spell state mana args))
                            :cost mana
                            :action action}))
                  spell-book))))

;;; Finding lowest cost (mana)

;; NOTE: if we wanted to know the lowest-mana sequence of turns we could modify
;; so that instead of storing only the accumulating cost (as the value) in the
;; priority-map, we could store a map of {:cost 0 :prior node} which points back
;; to the node that got us here. Then on the `wins?` condition we unwind and
;; return the path.

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
