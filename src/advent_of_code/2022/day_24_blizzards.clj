(ns advent-of-code.2022.day-24-blizzards
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.vectors :as v]
            [clojure.set :as set]))

(defn parse [input]
  (let [{:keys [locmap size]} (common/locmap<- input)
        finder (fn [targ-row]
                 (->> locmap
                      (keep (fn [[[r c] v]]
                              (when (and (= \. v) (= r targ-row))
                                [r c])))
                      first))
        [rows cols] size]
    {:start (finder 0)
     :end (finder (dec (first size)))
     :rows (- rows 2) :cols (- cols 2)
     :blizzards (update-vals (group-by val locmap) #(set (map first %)))}))

(defn mod-1 [num div] (inc (mod (dec num) div)))

(defn nexter [{:keys [start end rows cols blizzards]}]
  (let [clamper (fn [idx wid]
                  (fn [loc off s]
                    (update loc idx #(mod-1 (+ % (* s off)) wid))))
        clamp-r (clamper 0 rows)
        clamp-c (clamper 1 cols)
        windy? (fn [t loc]
                 (keep (fn [[b [f s]]] (get-in blizzards [b (f loc t s)]))
                       {\> [clamp-c -1] \< [clamp-c +1]
                        \v [clamp-r -1] \^ [clamp-r +1]}))]
    (fn [t loc]
      (for [∆ [[0 0] [-1 0] [1 0] [0 -1] [0 1]]
            :let [[r c :as loc] (v/add loc ∆)]
            :when (or (= loc start) (= loc end)
                      (and (<= 1 r rows) (<= 1 c cols)
                           (empty? (windy? (inc t) loc))))]
        [r c]))))

(defn route [t {:as info :keys [start end]}]
  (let [nexts (nexter info)]
    (loop [t t locs #{start}]
      (if (some #{end} locs)
        t
        (recur (inc t) (set (mapcat (partial nexts t) locs)))))))

(comment
  ;; puzzle 1 -- just under 2s
  (time
   (let [info (parse (slurp "input/2022/24-blizzards.txt"))]
     (route 0 info))) ; => 245

  ;; puzzle 2 -- 6.3s
  (time
   (let [info (parse (slurp "input/2022/24-blizzards.txt"))]
     (-> 0
         (route info)
         (route (set/rename-keys info {:start :end :end :start}))
         (route info))))
  )

;; Some thoughts:
;;
;;    * I don't need to use dijkstra. Just a standard BFS.
;;
;;    * I don't need to remap the blizzard locations every step. I can just use
;;      some modulo arithmetic to determine if any blizzards at time t will be
;;      in a certain spot.
