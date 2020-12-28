(ns advent-of-code.2015.day-3
  (:require [clojure.set]))

(defn add-next-house [{:keys [loc] :as info} delta]
  (let [next-house (map + loc delta)]
    (-> info
        (assoc :loc next-house)
        (update :visited conj next-house))))

(defn get-houses [in]
  (->> in
       (map {\^ [0 1] \> [1 0] \< [-1 0] \v [0 -1]})
       (reduce add-next-house {:loc '(0 0) :visited #{'(0 0)}})))

(defn puzzle1 [in]
  (->> in
       get-houses
       :visited
       count))

(comment
  (let [input ">"] (puzzle1 input))
  (let [input "^>v<"] (puzzle1 input))
  (let [input "^v^v^v^v^v"] (puzzle1 input))
  
  (let [input (slurp "input/2015/3-houses.txt")] (puzzle1 input)))

(defn puzzle2 [in]
  (->> (partition-all 2 in)
       ((juxt (partial map first) (partial map second)))
       (map get-houses)
       (map :visited)
       (apply clojure.set/union)
       count))

(comment
  (let [input ">"] (puzzle2 input))
  (let [input "^>v<"] (puzzle2 input))
  (let [input "^v^v^v^v^v"] (puzzle2 input))
  
  (let [input (slurp "input/2015/3-houses.txt")] (puzzle2 input)))