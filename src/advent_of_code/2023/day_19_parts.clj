(ns advent-of-code.2023.day-19-parts
  (:require [advent-of-code.common :as common]
            [advent-of-code.common2 :as common2]
            [clojure.string :as str]))

(defn parse-rule [r]
  (if-let [[_ c o v w] (re-matches #"(.*)(<|>)(\d+):(.*)" r)]
    [c o (parse-long v) w]
    r))

(defn parse-workflow [w]
  (let [[name rules] (str/split w #"\{|\}")]
    [name (map parse-rule (str/split rules #","))]))

(defn parse-rating [r]
  (zipmap ["x" "m" "a" "s"] (common/parse-longs r)))

(defn apply-rules [workflow]
  (fn this
    ([part] (this (workflow "in") part))
    ([[[c o v w :as r] & r's] part]
     (if (string? r)
       (if (#{"A" "R"} r)
         [r part]
         (this (workflow r) part))
       (if (({"<" <, ">" >} o) (part c) v)
         (this (workflow w [w]) part)
         (this r's part))))))

(defn find-ranges [workflow]
  (fn this
    ([part] (this (workflow "in") part))
    ([[[c o v w :as r] & r's] part]
     (if (string? r)
       (if (#{"A" "R"} r)
         [[r part]]
         (this (workflow r) part))
       (let [[s e] (part c)
             jmp (workflow w [w])]
         (if (= "<" o)
           (concat (this jmp (assoc part c [s (min e (dec v))]))
                   (this r's (assoc part c [(max s v) e])))
           (concat (this jmp (assoc part c [(max s (inc v)) e]))
                   (this r's (assoc part c [s (min v e)])))))))))

(comment
  (let [[workflows parts] (sequence (common2/split-grouped-lines) (slurp "input/2023/19-sample.txt"))]
    (def workflows (into {} (map parse-workflow) workflows))
    (def parts (map parse-rating parts)))
  (let [[workflows parts] (sequence (common2/split-grouped-lines) (slurp "input/2023/19-parts.txt"))]
    (def workflows (into {} (map parse-workflow) workflows))
    (def parts (map parse-rating parts)))

  ;; year 2023 day 19 puzzle 1
  (transduce (comp (map (apply-rules workflows))
                   (keep (fn [[r p]] (when (#{"A"} r) (vals p))))
                   (mapcat identity))
             +
             parts)
  ;; => 374873

  ;; year 2023 day 19 puzzle 2
  (transduce (comp (keep (fn [[r p]] (when (#{"A"} r) (vals p))))
                   (map #(transduce (map (fn [[s e]] (inc (- e s)))) * %)))
             +
             ((find-ranges workflows) {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [1 4000]}))
  ;; => 122112157518711
  )
