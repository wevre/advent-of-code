(ns advent-of-code.2022.day-06-packets)

(defn detect [len input]
  (+ len (->> input
              (partition len 1)
              (take-while #(apply (complement distinct?) %))
              count)))

(comment
  ;; puzzle 1
  (detect 4 (slurp "input/2022/06-packets.txt"))   ; => 1356

  ;; puzzle 2
  (detect 14 (slurp "input/2022/06-packets.txt"))   ; => 2564

  (detect 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  )
