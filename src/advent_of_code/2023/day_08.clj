(ns advent-of-code.2023.day-08
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]
            [advent-of-code.common.remainders :as remainders]))

(defn parse-input [input]
  (let [[[instr] lines] (common/split-grouped-lines input)
        nodes (into {} (for [line lines
                             :let [[n l r] (re-seq #"[A-Z]{3}" line)]]
                         [n {\L l \R r}]))]
    [instr nodes]))

(defn find-steps [init instr nodes lim]
  (loop [[s & s's] (cycle instr) n init i 0 cyc []]
    (let [end? (str/ends-with? n "Z")
          cyc (cond-> cyc end? (conj i))]
      (if (and end? (= (count cyc) lim))
        [init n cyc]
        (recur s's (get-in nodes [n s]) (inc i) cyc)))))

(comment
  (def input (slurp "input/2023/08-nodes.txt"))
  (def input (slurp "input/2023/08-sample2-nodes.txt"))
  (def input (slurp "input/2023/08-sample3-nodes.txt"))

  ;; `def`-ing the instructions and the nodes.
  (let [[i's n's] (parse-input input)]
    (def instr i's)
    (def nodes n's))

  ;; year 2023 day 08 puzzle 1
  (find-steps "AAA" instr nodes 1)
  ;; => ["AAA" "ZZZ" [17287]]
  ;;                  ^^^^^

  ;; Finding all 'start' nodes.
  (def start's (->> nodes keys (filter #(str/ends-with? % "A"))))
  ;; => ("BBA" "GTA" "VDA" "AAA" "GPA" "VSA")

  ;; Checking that all of these cycle around.
  (for [s start's
        :let [[n e [l :as steps]] (find-steps s instr nodes 5)]]
    [n e l (map #(/ % l) steps)])
  ;; => (["BBA" "SKZ" 19631 (1 2 3 4 5)]
  ;;     ["GTA" "FPZ" 20803 (1 2 3 4 5)]
  ;;     ["VDA" "STZ" 23147 (1 2 3 4 5)]
  ;;     ["AAA" "ZZZ" 17287 (1 2 3 4 5)]
  ;;     ["GPA" "CVZ" 13771 (1 2 3 4 5)]
  ;;     ["VSA" "MKZ" 17873 (1 2 3 4 5)])
  ;;                         ^^^^^^^^^ confirms regular cycles

  ;; Get the cycle lengths
  (for [s start's
        :let [[_n _e [l]] (find-steps s instr nodes 1)]]
    l)
  ;; => (19631 20803 23147 17287 13771 17873)

  ;; year 2023 day 08 puzzle 2
  ;; Find the lcm of the cycle lengths.
  (reduce remainders/lcm [19631 20803 23147 17287 13771 17873])
  ;; => 18625484023687

  ;; Curious, how many cycles for each node?
  (map #(/ 18625484023687 %) [19631 20803 23147 17287 13771 17873])
  ;; => (948779177 895326829 804660821 1077427201 1352514997 1042101719)
  ;; Hmmm. Yeah. Doing this by hand would take a while.

  )