(ns advent-of-code.2022.day-20-gps
  (:require [advent-of-code.common :as common]))

(defprotocol ICircleNode
  (value [this])
  (get-prev [this])
  (get-next [this])
  (set-prev [this node])
  (set-next [this node])
  (insert-node [this node])
  (remove-node [this]))

(deftype CircleNode [value ^:volatile-mutable prev ^:volatile-mutable next]
  ICircleNode
  (value [_] value)
  (get-prev [_] prev)
  (get-next [_] next)
  (set-prev [this node] (set! prev node) this)
  (set-next [this node] (set! next node) this)
  (insert-node [this node]
    (set-prev node this)
    (set-next node next)
    (set-prev next node)
    (set-next this node))
  (remove-node [this]
    (let [old-prev prev]
      (set-prev next prev)
      (set-next prev next)
      (set-prev this this)
      (set-next this this)
      old-prev)))

(defn circle-node
  "A single node that points to itself."
  [value]
  (let [node (->CircleNode value nil nil)]
    (set-next node node)
    (set-prev node node)))

(defn build-nodes
  "Return a vector of linked nodes"
  [values]
  (loop [nodes-vec [] nodes-map {} last nil [v & vs] values]
    (if (nil? v)
      [nodes-vec nodes-map]
      (let [node (circle-node v)]
        (when last (insert-node last node))
        (recur (conj nodes-vec node) (assoc nodes-map v node) node vs)))))

(defn- node-info [n] [(value n) (value (get-prev n)) (value (get-next n))])

(defn mix [nodes]
  (let [cnt (dec (count nodes))]
    (doseq [n nodes
            :let [v (mod (value n) cnt)]]
      (insert-node (nth (iterate get-next (remove-node n)) v) n))))

(defn- get-nth [node index]
  (nth (iterate get-next node) index))

(defn solve [input d-key iters]
  (let [[nodes-vec nodes-map] (->> input
                                   common/parse-longs
                                   (map #(* % d-key))
                                   build-nodes)
        _ (doseq [_ (range iters)] (mix nodes-vec))
        zeroth (get nodes-map 0)]
    (->> [1000 2000 3000]
         (map #(mod % (count nodes-vec)))
         (map #(get-nth zeroth %))
         (map value)
         (apply +))))

(def decryption-key 811589153)

(comment
  ;; puzzle 1
  (let [input "1,2,-3,3,-2,0,4" #_(slurp "input/2022/20-grove-ps.txt")]
    (solve input 1 1))   ; => 10707

  ;; puzzle 2
  (let [input #_"1,2,-3,3,-2,0,4" (slurp "input/2022/20-grove-ps.txt")]
    (solve input decryption-key 10))   ; => 2488332343098
  )
