(ns advent-of-code.2020.day-15)

(defn puzzle [n in]
  (let [parse (map #(Long/parseLong %) (re-seq #"\d+" in))
        seed (map-indexed #(vector %2 (list %1 %1)) parse)]
    (:res (loop [acc (into {:res (last parse)} seed) i (count seed)]
            (if (< i n)
              (let [x (reduce - (acc (:res acc) '(0 0)))
                    acc (-> (assoc acc :res x)
                            (assoc x (take 2 (cons i (acc x (list i))))))]
                (recur acc (inc i)))
              acc)))))


(comment
  (puzzle 2020 "8,0,17,4,1,12")
  (puzzle 30000000 "8,0,17,4,1,12")

  (map #(puzzle 2020 %) '("0,3,6" "1,3,2" "2,1,3" "1,2,3" "2,3,1" "3,2,1" "3,1,2"))
  (map #(puzzle 30000000 %) '("0,3,6" "1,3,2" "2,1,3" "1,2,3" "2,3,1" "3,2,1" "3,1,2")))