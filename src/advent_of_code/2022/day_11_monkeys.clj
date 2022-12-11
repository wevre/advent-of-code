(ns advent-of-code.2022.day-11-monkeys
  (:require [advent-of-code.common :as common]
            [clojure.edn :as edn]
            [clojure.java.math :as math]))

(def evaluate
  (memoize
   (fn [old form]
     (eval (map #(if (= % 'old) old %) form)))))

(defn parse-simian [[a b c & d]]
  (let [i (parse-long (re-find #"\d+" a))
        items (into [] (map bigint (common/parse-longs b)))
        [s1 op s2] (edn/read-string (str "(" (re-find #"old.*$" c) ")"))
        [div t-to f-to] (map #(parse-long (re-find #"\d+" %)) d)]
    [i {:items items :op (list op s1 s2) :div div :t-to t-to :f-to f-to
        :count 0}]))

(defn parse [input]
  (->> input
       common/split-grouped-lines
       (map parse-simian)
       (into {})))

(defn round [worry]
  (fn [state]
    (let [rf (fn [state i]
               (let [{:keys [items op div t-to f-to]} (get state i)
                     {t-items true f-items false}
                     (->> items
                          (map #(evaluate % op))
                          (map worry)
                          (group-by #(zero? (mod % div))))]
                 (-> state
                     (assoc-in [i :items] [])
                     (update-in [i :count] + (count items))
                     (update-in [t-to :items] into t-items)
                     (update-in [f-to :items] into f-items))))]
      (reduce rf state (range 8)))))

(defn monkey-business [state n worry]
  (->> state
       (iterate (round worry))
       (drop n)
       first
       vals
       (map :count)
       (sort >)
       (take 2)
       (apply *)))

(comment

  (evaluate 52 (edn/read-string "(* old old)"))

  ;; puzzle 1
  (let [state (parse (slurp "input/2022/11-monkeys.txt"))
        worry (fn [v] (bigint (math/floor (/ v 3))))]
    (monkey-business state 20 worry))   ; => 58786

  ;; puzzle 2
  (let [state (parse (slurp "input/2022/11-monkeys.txt"))
        lcm (->> state vals (map :div) (apply *))
        worry (fn [v] (mod v lcm))]
    (monkey-business state 10000 worry))   ; => 14952185856
  )