(ns advent-of-code.2023.day-14-rocks
  (:require [advent-of-code.common2 :as common2]))

(defn scoot [loc's init-r +loc rocks]
  (loop [[loc & loc's] loc's stop init-r rocks rocks]
    (if-not loc
      rocks
      (let [entry (get rocks loc)]
        (case entry
          \# (recur loc's loc rocks)
          \O (let [r (+loc stop)]
               (recur loc's r (-> rocks
                                  (dissoc loc)
                                  (assoc r entry))))
          (recur loc's stop rocks))))))

(defn inc-er [d] (fn [loc] (mapv + loc d)))

(defn scoot-dir [i's j's-er init-er +loc rocks]
  (reduce (fn [rocks i] (scoot (j's-er i) (init-er i) +loc rocks))
          rocks
          i's))

(defn score [rocks [rows cols]]
  (reduce + (for [r (range rows)
                  c (range cols)
                  :let [entry (get rocks [r c])]
                  :when (= \O entry)]
              (- rows r))))

(defn print-rocks [rocks [rows cols]]
  (doseq [r (range rows)]
    (doseq [c (range cols)]
      (print (if-let [rock (rocks [r c])] rock \.)))
    (println))
  (println))

(defn dirs [[rows cols]]
  [{:dir :north
    :i's (range cols)
    :j's-er (fn [i] (for [r (range rows)] [r i]))
    :init-er (fn [i] [-1 i])
    :+loc (inc-er [1 0])}
   {:dir :west
    :i's (range rows)
    :j's-er (fn [i] (for [c (range cols)] [i c]))
    :init-er (fn [i] [i -1])
    :+loc (inc-er [0 1])}
   {:dir :south
    :i's (range cols)
    :j's-er (fn [i] (for [r (range rows)] [(- rows r 1) i]))
    :init-er (fn [i] [rows i])
    :+loc (inc-er [-1 0])}
   {:dir :east
    :i's (range rows)
    :j's-er (fn [i] (for [c (range cols)] [i (- cols c 1)]))
    :init-er (fn [i] [i cols])
    :+loc (inc-er [0 -1])}])

(defn rock-cycle [dir's]
  (fn [rocks]
    (reduce (fn [rocks {:keys [i's j's-er init-er +loc]}]
              (scoot-dir i's j's-er init-er +loc rocks))
            rocks
            dir's)))

(comment
  (do
    (def rocks (into {} (common2/locmap<- #{\# \O}) (slurp "input/2023/14-sample.txt")))
    (def size [10 10]))
  (do
    (def rocks (into {} (common2/locmap<- #{\# \O}) (slurp "input/2023/14-rocks.txt")))
    (def size [100 100]))

  ;; year 2023 day 14 puzzle 1
  (let [dir's (dirs size)
        scooted ((rock-cycle (take 1 dir's)) rocks)]
    (score scooted size))
  ;; => 108759

  ;; Let it run for a bit, then see if we get a repeating result.
  (let [cycle-er (rock-cycle (dirs size))
        out (->> (iterate cycle-er rocks)
                 (drop 155)
                 first)
        pattern (->> (iterate cycle-er out)
                     (take 50))]
    (map-indexed (fn [i r] [i (= r out)]) pattern))

  ;; For sample data, after running 50 times, we found a repeat period of 7
  ;; For puzzle data, after running 155 times, found a repeat period of 17

  ;; year 2023 day 14 puzzle 2
  (time
   (let [cycle-er (rock-cycle (dirs size))
         prime 155
         period 17
         align (rem (- 1000000000 prime) period)
         billionth (->> (iterate cycle-er rocks)
                  (drop (+ prime align))
                  first)]
     (score billionth size)))
  ;; => 89089

  )
