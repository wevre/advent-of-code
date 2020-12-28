(ns advent-of-code.2015.day-1)

(defn puzzle1 [in]
  (->> in
       (map {\( +1 \) -1})
       (reduce + 0)))

(comment
  (let [input "))((((("] (puzzle1 input))
  
  (let [input ")())())"] (puzzle1 input))

  (let [input (slurp "input/2015/1-floors.txt")]
    (puzzle1 input)))

(defn puzzle2 [in]
  (->> in
       (map {\( +1 \) -1})
       (reductions + 0)
       (take-while #(not= -1 %))
       count))

(comment
  (let [input ")"] (puzzle2 input))
  
  (let [input "()())"] (puzzle2 input))
  
  (let [input (slurp "input/2015/1-floors.txt")]
    (puzzle2 input)))
