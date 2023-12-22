(ns advent-of-code.common2
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn split-grouped-lines []
  (fn [xf]
    (let [run (volatile! nil)
          grp (volatile! nil)]
      (fn
        ([] (xf))
        ([result]
         (when-let [curr-run @run] (vswap! grp (fnil conj []) (str/join curr-run)))
         (when-let [curr-grp @grp] (xf result curr-grp))
         (xf result))
        ([result input]
         (if (= input \newline)
           (let [curr-run @run
                 curr-grp @grp]
             (cond
               curr-run (do (vswap! grp (fnil conj []) (str/join curr-run))
                            (vreset! run nil)
                            result)
               curr-grp (do (vreset! grp nil)
                            (xf result curr-grp))
               :else result))
           (do (vswap! run (fnil conj []) input)
               result)))))))

(comment
  (sequence (split-grouped-lines) "\n\n123\n\n\n456\n789\n\nabc\ndef\nghi\n\n"))

(defn locmap<-
  ([] (locmap<- identity))
  ([keep-fn & {:keys [?size] :or {?size false}}]
   (fn [rf]
     (let [rv (volatile! 0)
           cv (volatile! 0)
           cm (volatile! 0)]
       (fn
         ([] (rf))
         ([result]
          (when ?size (rf result [:size [@rv @cm]]))
          (rf result))
         ([result input]
          (if (= \newline input)
            (do
              (vswap! rv inc)
              (vreset! cv 0)
              result)
            (let [rval @rv
                  cval @cv]
              (vswap! cv inc)
              (vswap! cm max (inc cval))
              (if-let [kept (keep-fn input)]
                (rf result [[rval cval] kept])
                result)))))))))

(comment
  (into {} (locmap<- #{\| \- \F \J \L \7}) (slurp "input/2023/10-sample-pipes.txt")))
