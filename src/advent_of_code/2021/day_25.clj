(ns advent-of-code.2021.day-25
  (:require [clojure.string :as str]))

;;;; --- Day 25: Sea Cucumber ---
;;;; https://adventofcode.com/2021/day/25

(def rows 137)
(def cols 139)

(defn parse-input [s]
  (->> (str/split-lines s)
       (map-indexed vector)
       (mapcat (fn [[r l]] (map-indexed (fn [c v] [[r c] v]) l)))
       (remove #(#{\.} (second %)))
       (into {})))

(defn can-move? [herd type axis wrap]
  (fn [loc v]
    (let [spot (update loc axis #(mod (inc %) wrap))]
      (cond
        (not= type v) false
        (contains? herd spot) false
        :else spot))))

(defn move [[edit? herd] type axis wrap]
  (let [get-move (can-move? herd type axis wrap)]
    (reduce-kv (fn [[edit? herd] k v]
                 (if-let [spot (get-move k v)]
                   [(or edit? true) (assoc herd spot v)]
                   [edit? (assoc herd k v)]))
               [edit? {}]
               herd)))

(defn print-herd [[_ herd]]
  (doseq [r (range rows)]
    (doseq [c (range cols)]
      (print (herd [r c] \.)))
    (println)))

(defn step [[_edit? herd]]
  (-> [false herd]
      (move \> 1 cols)
      (move \v 0 rows)))

(comment
  ;; part 1 -- ~6s
  (time
   (let [input (parse-input (slurp "input/2021/25-cukes.txt"))]
     (->> (iterate step [true input])
          (take-while #(first %))
          count)))   ;=> 530

  (with-redefs [rows 9 cols 10]
    (let [input (parse-input "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")]
      (->> (iterate step [true input])
           (drop 10)
           first
           (print-herd))))
  )