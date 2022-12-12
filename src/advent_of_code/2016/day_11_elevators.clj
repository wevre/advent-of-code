(ns advent-of-code.2016.day-11-elevators
  (:require [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def init {:E 1
           :F {1 #{[:Po :G]
                   [:Tm :G] [:Tm :M]
                   [:Pm :G]
                   [:Ru :G] [:Ru :M]
                   [:Co :G] [:Co :M]}
               2 #{[:Po :M]
                   [:Pm :M]}
               3 #{}
               4 #{}}})

(def init-sample {:E 1
                  :F {1 #{[:H :M] [:L :M]}
                      2 #{[:H :G]}
                      3 #{[:L :G]}
                      4 #{}}})

;; TODO: we're safe if a :M chip is connected to its :G power source or if there
;; are no other :G's around. So it's like:
;; every :M has it's matching :G? SAFE
;; else there are no :G's? SAFE
(defn floor-safe? [floor]
  (let [chips (into #{} (keep (fn [[a b]] (when (= :M b) a)) floor))
        generators (into #{} (keep (fn [[a b]] (when (= :G b) a)) floor))]
    (or (empty? (set/difference chips generators))
        (empty? generators))))

(comment
  (mapcat #(combo/combinations [1 2 3 4] %) [1 2])

  (floor-safe? #{#_[:H :G] #_[:L :G] [:L :M] [:H :M]})
  (floor-safe? [[:G :Pm] [:M :Tm]]))

(defn cost [{floors :F}]
  (+ (* 3 (count (get floors 1)))
     (* 2 (count (get floors 2)))
     (* 1 (count (get floors 3)))))

(defn moves [{i :E floors :F}]
  (for [items (mapcat #(combo/combinations (vec (floors i)) %) [1 2])
        :when (floor-safe? items)
        :let [src (set/difference (floors i) (set items))]
        :when (floor-safe? src)
        j [(dec i) (inc i)]
        :when (<= 1 j 4)
        :let [dest (set/union (floors j) (set items))]
        :when (floor-safe? dest)]
    {:E j :F (assoc floors i src j dest)}))

(defrecord State [info]
  dijkstra/IState
  (state-key [_this] info)
  (cost [_this] (cost info))
  (next-states [_this] (->> (moves info) (map #(->State %))))
  (end? [_this] (and (empty? (get-in info [:F 1]))
                     (empty? (get-in info [:F 2]))
                     (empty? (get-in info [:F 3])))))

(defn solve [state]
  (let [{:keys [node costs visited]} (dijkstra/lowest-cost (->State state))]
    (println "size of visited: " (count visited))
    (println "size of priority map 'costs': " (count costs))
    (->> node dijkstra/path butlast count)))

(comment
  ;; puzzle 1 -- about 12s -- over 157,000 moves considered
  (time
   (solve init))   ; => 47

  ;; puzzle 2 -- over 12m! -- over 6,028,000 routes considered
  (time
   (solve (update-in init [:F 1] into #{[:E :G] [:E :M] [:D :G] [:D :M]})))   ; => 71
  )
