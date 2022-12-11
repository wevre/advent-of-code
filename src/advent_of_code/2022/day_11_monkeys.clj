(ns advent-of-code.2022.day-11-monkeys
  (:require [advent-of-code.common :as common]
            [clojure.edn :as edn]))

;; NOTE: I like @zelark's approach to capture `throw-to` as a function created
;; when parsing the monkey, instead of storing the two recipients. BTW: can't
;; call it by the most natural choice, 'throw', because `throw` is a special
;; form. I actually ended up calling it 'whom' because I think it's funny.

(defn parse-simian [[a b c & d]]
  (let [i (parse-long (re-find #"\d+" a))
        items (into [] (common/parse-longs b))
        [s1 op s2] (edn/read-string (str "(" (re-find #"old.*$" c) ")"))
        [div m1 m2] (map #(parse-long (re-find #"\d+" %)) d)]
    [i {:items items
        :op (fn [x] (apply (resolve op) (map #({'old x} % %) [s1 s2])))
        :whom #(if (zero? (mod % div)) m1 m2)
        :div div
        :count 0}]))

(defn parse [input]
  (->> input
       common/split-grouped-lines
       (map parse-simian)
       (into {})))

(defn round [worry cnt]
  (fn [state]
    (reduce (fn [state i]
              (let [{:keys [items op whom]} (get state i)
                    chuck (fn [state item]
                            (update-in state [(whom item) :items] conj item))]
                (-> (reduce chuck state (map (comp worry op) items))
                    (assoc-in [i :items] [])
                    (update-in [i :count] + (count items)))))
            state
            (range cnt))))

(defn monkey-business [state n worry]
  (->> state
       (iterate (round worry (count state)))
       (drop n)
       first
       vals
       (map :count)
       (sort >)
       (take 2)
       (apply *)))

(comment
  ;; puzzle 1
  (let [state (parse (slurp "input/2022/11-monkeys.txt"))
        worry (fn [v] (quot v 3))]
    (monkey-business state 20 worry))   ; => 58786

  ;; puzzle 2
  (let [state (parse (slurp "input/2022/11-monkeys.txt"))
        lcm (->> state vals (map :div) (apply *))
        worry (fn [v] (mod v lcm))]
    (monkey-business state 10000 worry))   ; => 14952185856
  )
