(ns advent-of-code.2015.day-11)

(defn inc-pswd [p]
  (loop [i (dec (count p)) p p]
    (case (p i)
      122 (recur (dec i) (assoc p i 97))
      (104 107 110) (update p i #(+ 2 %))
      (update p i inc))))

(defn num-pairs [p]
  (->> p
       (partition-by identity)
       (filter #(<= 2 (count %)))
       (map first)
       set
       count))

(defn num-pairs* [p]
  (->> (partition 2 1 p)
       (filter #(apply = %))
       set
       count))

(defn longest-run [p]
  (let [counts (->> p
                    (partition 2 1)
                    (map (comp (partial apply -) reverse))
                    (partition-by identity)
                    (filter (comp #{1} first))
                    (map count))]
    (if (seq counts) (reduce max counts) 0)))

(defn valid-pswd? [p]
  (and
   (not-any? #{105 108 110} p)
   (< 1 (num-pairs p))
   (< 1 (longest-run p))))

(defn puzzle1 [in]
  (->> in
       (mapv int)
       (iterate inc-pswd)
       (drop 1)
       (filter valid-pswd?)
       first
       (map char)
       (apply str)))

(comment
  (let [input "hepxcrrq"] (puzzle1 input))
  
  (let [input "hepxcrrq"] (puzzle1 (puzzle1 input))))