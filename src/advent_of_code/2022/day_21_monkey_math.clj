(ns advent-of-code.2022.day-21-monkey-math
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent-of-code.common :as common]))

(defn parse [input]
  (->> (str/replace input ":" "")
       str/split-lines
       (map #(str "(" % ")"))
       (map edn/read-string)
       (reduce (fn [env [a b c d]] (assoc env a (if c (vector c b d) b))) {})))

(defn evaluator [env]
  (common/z-combinator
   (fn [f sym]
     (let [v (get env sym sym)]
       (cond
         (integer? v) v
         (symbol? v) (f v)
         :else (apply (eval (first v)) (map f (rest v))))))))

(comment
  ;; puzzle 1
  (let [env (parse (slurp "input/2022/21-monkey-math.txt"))]
    ((evaluator env) 'root))   ; => 93813115694560

  ;; puzzle 2
  (let [env (parse (slurp "input/2022/21-monkey-math.txt"))
        env0 (-> env (assoc-in ['root 0] '-) (assoc 'humn 0))
        env1 (-> env (assoc-in ['root 0] '-) (assoc 'humn 1))
        root0 ((evaluator env0) 'root)
        root1 ((evaluator env1) 'root)]
    (long (/ (- root0) (- root1 root0)))   ; => 3910938071092
    )
  )

;; 2022-12-29
;;
;;    Was thinking of doing part 2 with core logic, which I have used in other
;;    AoC puzzles and I know it can work. But read through the slack channel and
;;    @Callum Oakley (and others) pointed out that root is a linear function of
;;    h. So pick two values for h, generate root for each, find slope and
;;    intercept of line and solve for value you need (x-intercept).
