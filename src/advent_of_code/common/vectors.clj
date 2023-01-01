(ns advent-of-code.common.vectors)

(defn add [& vs] (apply mapv + vs))

(defn mult [& vs] (apply mapv * vs))
