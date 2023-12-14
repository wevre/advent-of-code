(ns advent-of-code.common2)

(defn split-grouped-lines []
  (fn [xf]
    (let [run (volatile! nil)
          grp (volatile! nil)]
      (fn
        ([] (xf))
        ([result]
         (when-let [curr-run @run] (vswap! grp (fnil conj []) curr-run))
         (when-let [curr-grp @grp] (xf result curr-grp))
         (xf result))
        ([result input]
         (if (= input \newline)
           (let [curr-run @run
                 curr-grp @grp]
             (cond
               curr-run (do (vswap! grp (fnil conj []) curr-run)
                            (vreset! run nil)
                            result)
               curr-grp (do (vreset! grp nil)
                            (xf result curr-grp))
               :else result))
           (do (vswap! run (fnil conj []) input)
               result)))))))

(comment
  (sequence (split-grouped-lines) "\n\n123\n\n\n456\n789\n\nabc\ndef\nghi\n\n"))
