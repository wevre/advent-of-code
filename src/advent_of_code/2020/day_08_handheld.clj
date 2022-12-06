(ns advent-of-code.2020.day-08-handheld
  (:require [clojure.string :as str]))

;; --- Day 8: Handheld Halting ---

(defn parse [line]
  (let [[instr offset] (str/split line #" ")]
    {:instr (keyword instr)
     :arg (Integer/parseInt offset)}))

(defn next-i [i {:keys [instr arg]}]
  (cond-> i
    (#{:nop :acc} instr) inc
    (#{:jmp} instr) (+ arg)))

(defn next-acc [acc {:keys [instr arg]}]
  (cond-> acc
    (#{:acc} instr) (+ arg)))

(defn execute [instructions]
  (loop [i 0 acc 0 visited? #{}]
    (cond
      (visited? i) {:nonterm acc}
      (>= i (count instructions)) {:term acc}
      :else (let [instr (instructions i)]
              (recur (next-i i instr)
                     (next-acc acc instr)
                     (conj visited? i))))))

(defn puzzle1 [in]
  (->> in
       (str/split-lines)
       (mapv parse)
       execute
       :nonterm))

(comment
  (let [input (slurp "input/2020/handheld.txt")]
    (puzzle1 input))

  (let [input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"]
    (puzzle1 input)))

(defn mangle-nop [instructions]
  (for [[i {instr :instr}] (map-indexed vector instructions)
        :when (= :nop instr)]
    (assoc-in instructions [i :instr] :jmp)))

(defn mangle-jmp [instructions]
  (for [[i {instr :instr}] (map-indexed vector instructions)
        :when (= :jmp instr)]
    (assoc-in instructions [i :instr] :nop)))

(defn puzzle2 [in]
  (->> in
       (str/split-lines)
       (mapv parse)
       ((juxt mangle-nop mangle-jmp))
       (apply concat)
       (map #(execute %))
       (some :term)))

(comment
  (let [input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"]
    (puzzle2 input))

  (let [input (slurp "input/2020/handheld.txt")]
    (puzzle2 input))
  )