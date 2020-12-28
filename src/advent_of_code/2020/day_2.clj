(ns advent-of-code.2020.day-2)

(defn valid-password-1? [[l h c s]]
  (<= l ((frequencies s) c 0) h))

(defn valid-password-2? [[l h c s]]
  (not= (= c (nth s (dec l))) (= c (nth s (dec h)))))

(defn parse [[l h c s]]
  [(Integer/parseInt l) (Integer/parseInt h) (first c) s])

(defn puzzle [in pred]
  (->> (re-seq #"[\d\w]+" in)
       (partition 4)
       (map parse)
       (filter pred)
       count))

(comment
  (puzzle (slurp "input/2020/2-passwords.txt") valid-password-1?)
  
  (puzzle (slurp "input/2020/2-passwords.txt") valid-password-2?)
  
  (let [input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"]
    [(puzzle input valid-password-1?)
     (puzzle input valid-password-2?)])
  )
