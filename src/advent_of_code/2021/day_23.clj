(ns advent-of-code.2021.day-23
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-input [s]
  {:cost 0
   :burrow (->> (str/split-lines s)
                (map-indexed vector)
                (mapcat (fn [[r l]]
                          (map-indexed (fn [c v] [[r c] v]) l)))
                (into {}))})

(def energy {\A 1 \B 10 \C 100 \D 1000})

(def room-column {\A 3 \B 5 \C 7 \D 9})

(defn valid-move [[r-from c-from] [r-dest c-dest] pod burrow]
  (cond
    (= r-from r-dest 1) false   ; no wandering in hallway
    (and (= 3 r-from) (not= \. (burrow [2 c-from]))) false   ; blocked room
    (and (= 3 r-from) (= c-from (room-column pod))) false   ; stay put!
    (and (= 2 r-from) (= c-from (room-column pod)) (= pod (burrow [3 c-from]))) false   ; done!
    (and (= 2 r-dest) (= \. (burrow [3 c-dest]))) false   ; go all the way back
    (and (= 2 r-dest) (not= pod (burrow [3 c-dest]))) false   ; stranger
    (and (<= 2 r-dest 3) (not= (room-column pod) c-dest)) false   ; not your room
    (and (= 1 r-dest) (#{3 5 7 9} c-dest)) false   ; don't linger in front of room
    :else (every? #{\.} (map #(burrow [1 %])
                             (if (< c-from c-dest)
                               (range (inc c-from) (inc c-dest))
                               (range c-dest c-from)))))
  )

(defn distance [[r-from c-from] [r-dest c-dest]]
  (+
   (if (and (<= 2 r-from 3) (<= 2 r-dest 3))
     (+ (dec r-from) (dec r-dest))
     (math/abs (- r-from r-dest)))
   (math/abs (- c-from c-dest))))

(defn moves [{burrow :burrow :as state}]
  (for [[from pod] burrow :when (#{\A \B \C \D} pod)
        dest (keys (filter #(= \. (val %)) burrow))
        :when (valid-move from dest pod burrow)
        :let [distance (distance from dest)]]
    (-> state
        (assoc-in [:burrow dest] pod)
        (assoc-in [:burrow from] \.)
        (update :cost + (* distance (energy pod))))))

(defn organized? [{burrow :burrow}]
  (every? true?
          (for [[pod col] room-column]
            (= pod (burrow [2 col]) (burrow [3 col])))))

(def organize
  (memoize
   (fn [state]
     (if (organized? state)
       (:cost state)
      (reduce min ##Inf (for [move (moves state)] (organize move)))))))

(defn print-state [{:keys [cost burrow]}]
  (println "Cost:" cost)
  (doseq [r (range 5)]
    (doseq [c (range 13)
            :let [v (burrow [r c])]
            :when (some? v)]
      (print (burrow [r c])))
    (println)))

(comment
  (let [input (parse-input "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########")]
    (organized? input))

  (time
   (let [input (parse-input (slurp "input/2021/23-amphipods.txt"))]
     (organize input)))
  )
