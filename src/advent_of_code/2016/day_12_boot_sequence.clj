(ns advent-of-code.2016.day-12-boot-sequence
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn execute [oprs regs]
  (loop [iptr 0 regs regs]
    (if (>= iptr (count oprs))
      regs
      (let [[op arg1 arg2] (get oprs iptr)]
        (case op
          cpy (recur (inc iptr) (assoc regs arg2 (or (regs arg1) arg1)))
          inc (recur (inc iptr) (update regs arg1 inc))
          dec (recur (inc iptr) (update regs arg1 dec))
          jnz (if (zero? (or (regs arg1) arg1))
                (recur (inc iptr) regs)
                (recur (+ iptr arg2) regs)))))))

(comment
  ;; puzzle 1
  (let [oprs (->> (slurp "input/2016/12-assembunny.txt")
                  str/split-lines
                  (map #(str "[" % "]"))
                  (mapv edn/read-string))
        regs {'a 0 'b 0 'c 0 'd 0}]
    (-> (execute oprs regs) (get 'a)))   ; => 318003

  ;; puzzle 2
  (let [oprs (->> (slurp "input/2016/12-assembunny.txt")
                  str/split-lines
                  (map #(str "[" % "]"))
                  (mapv edn/read-string))
        regs {'a 0 'b 0 'c 1 'd 0}]
    (-> (execute oprs regs) (get 'a)))   ; => 9227657
  )
