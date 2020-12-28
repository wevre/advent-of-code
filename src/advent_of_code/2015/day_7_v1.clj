(ns advent-of-code.2015.day-7-v1
  (:require [clojure.string :as str]))


(defn parse [input]
  (reduce (fn [m inst]
            (let [[a b c d e] (str/split inst #" ")]
              (cond
                (= b "->")     (assoc m c {:value a})
                (= a "NOT")    (assoc m d {:op bit-not :args [b]})
                (= b "AND")    (assoc m e {:op bit-and :args [a c]})
                (= b "OR")     (assoc m e {:op bit-or :args [a c]})
                (= b "LSHIFT") (assoc m e {:op bit-shift-left :args [a c]})
                (= b "RSHIFT") (assoc m e {:op unsigned-bit-shift-right :args [a c]}))))
          {}
          input))

(declare evaluate)

(def parse-value
  (memoize
   (fn [env value]
     (let [v (read-string value)]
       (if (integer? v)
         v
         (evaluate env value))))))

(defn evaluate [env k]
  (let [{:keys [op args value]} (get env k)]
    (if (nil? op)
      (parse-value env value)
      (apply op (map (partial parse-value env) args)))))

(defn solve [env]
  (bit-and 0xffff (evaluate env "a")))

(defn puzzle1 [in]
  (->> in
       str/split-lines
       parse
       solve))

(defn puzzle2 [in]
  (let [input (->> in
                   str/split-lines
                   parse)]
    (solve (assoc input "b" {:value (str (solve input))}))))

(comment
  (let [input (slurp "input/2015/7-wires.txt")]
    (puzzle1 input))
  
  (let [input (slurp "input/2015/7-wires.txt")]
    (puzzle2 input)))