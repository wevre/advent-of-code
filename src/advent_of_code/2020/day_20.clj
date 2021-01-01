(ns advent-of-code.2020.day-20
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 20: Jurassic Jigsaw ---

(def orient-map 
  "If oriented to <key>, what edge lies in each direction?"
  {:N {:N :N, :E :E, :S :S, :W :W}
   :E {:N :E, :E :S, :S :W, :W :N}
   :S {:N :S, :E :W, :S :N, :W :E}
   :W {:N :W, :E :N, :S :E, :W :S}
   :n {:N :n, :E :e, :S :s, :W :w}
   :e {:N :e, :E :s, :S :w, :W :n}
   :s {:N :s, :E :w, :S :n, :W :e}
   :w {:N :w, :E :n, :S :e, :W :s}})

(defn find-orient
  "If my `edge` is rotated to `dir`, what is my orientation?"
  [edge dir]
  (first (keep (fn [[k v]] (when (= dir (v edge)) k)) orient-map)))

(defn get-adj 
  "Return neighbor of `tile` in direction of (layout) `dir`. Current orientation
   of `tile` will be used to find which of its eight edges points to `dir` and 
   which image matches that edge. The adjacent image will be returned as a tile, 
   properly oriented to match input `tile`."
  [edge-map tile dir]
  (let [oth-dir ({:N :S :S :N :E :W :W :E} dir)
        keys [(:id tile) (get-in orient-map [(:orient tile) dir])]]
    (when-let [next (get-in edge-map keys)]
      {:id (:id next) :orient (find-orient oth-dir (:edge next))})))

(defn get-next 
  "Returns first tile that matches supplied `north` and `west` neighbors. Asking
   for a NW corner tile, with north and west neighbors both nil, will return 
   _all_ the corners, hence the built-in call to `first`."
  [edge-map north west]
  (first (for [id (keys edge-map) orient [:N :E :S :W :n :e :s :w]
               :let [tile {:id id :orient orient}]
               :when (and (= north (get-adj edge-map tile :N))
                          (= west (get-adj edge-map tile :W)))]
           tile)))
(defn image
  "Takes seq of strings defining a single puzzle (including id string) and 
   returns vector of id and image data, ready to pour into dictionary."
  [lines]
  (let [[id-str & rest] lines
        id (edn/read-string (re-find #"\d+" id-str))]
    [id (map seq rest)]))

(defn borders
  "Takes an id and an image and returns a list of 'borders'."
  [id image]
  [{:border (first image) :id id :edge :N}
   {:border (map last image) :id id :edge :E}
   {:border (reverse (last image)) :id id :edge :S}
   {:border (reverse (map first image)) :id id :edge :W}
   {:border (reverse (first image)) :id id :edge :n}
   {:border (map first image) :id id :edge :e}
   {:border (last image) :id id :edge :s}
   {:border (reverse (map last image)) :id id :edge :w}])

(defn edge-map [images]
  (let [unflip {:N :n :E :w :S :s :W :e :n :N :e :W :s :S :w :E}]
    (->> images
         (reduce-kv (fn [acc id img] (into acc (borders id img))) [])
         (group-by :border)
         (reduce-kv
          (fn [acc _k [{id1 :id e1 :edge} {id2 :id e2 :edge}]]
            (if id2
              (-> acc   ; See the notes for what is going on here.
                  (update id1 (fnil conj {}) {e1 {:id id2 :edge (unflip e2)}})
                  (update id1 (fnil conj {}) {(unflip e1) {:id id2 :edge e2}})
                  (update id2 (fnil conj {}) {e2 {:id id1 :edge (unflip e1)}})
                  (update id2 (fnil conj {}) {(unflip e2) {:id id1 :edge e1}}))
              acc))
          {}))))

(defn puzzle1 [input]
  (let [images (->> (str/split input #"\n\n")
                    (map str/split-lines)
                    (into {} (map image)))]
    (->> (edge-map images)
         (filter #(= 4 (count (val %))))
         (map key)
         (apply *))))

(comment
  (puzzle1 (slurp "input/2020/20-tiles.txt"))

  (puzzle1 (slurp "input/2020/20-example_tiles.txt")))

(defn rotate [tile]
  (when (not-any? empty? tile)
    (concat (rotate (map rest tile)) (list (map first tile)))))

(defn flip [tile] (map reverse tile))

(defn assemble [edge-map]
  (loop [len 0 acc [] line [] tile (get-next edge-map nil nil)]
    (cond
      (not tile) acc
      (or (empty? line)
          (and (zero? len) (not (= 4 (count (edge-map (:id tile))))))
          (< (count line) len))
      (recur len acc (conj line tile)
             (get-next edge-map (get (last acc) (inc (count line))) tile))
      :else
      (recur (count line) (conj acc (conj line tile)) []
             (get-next edge-map (first line) nil)))))

(def txf {:N identity
          :E rotate
          :S (comp rotate rotate)
          :W (comp rotate rotate rotate)
          :n flip
          :e (comp rotate flip)
          :s (comp rotate rotate flip)
          :w (comp rotate rotate rotate flip)})

(defn img<-tile [images {id :id orient :orient}]
  (let [crop (comp (partial drop 1) butlast)]
    (map crop (crop ((txf orient) (images id))))))

(def pattern #"..................#.#....##....##....###.#..#..#..#..#..#...")

(defn puzzle2 [input]
  (let [images (->> (str/split input #"\n\n")
                    (map str/split-lines)
                    (into {} (map image)))
        merged (->> (assemble (edge-map images))
                    (map #(map (partial img<-tile images) %))
                    (mapcat #(apply map concat %)))
        hashes (count (keep #{\#} (flatten merged)))]
    (- hashes 
       (* 15 (->> (for [[_ t] txf]
                    (->> (t merged)
                         (map #(partition 20 1 %))
                         (apply mapcat vector)
                         (partition 3 1)
                         (map (comp str/join flatten))
                         (keep #(re-matches pattern %))))
                  (keep (fn [matches] (when (seq matches) (count matches))))
                  first)))))

(comment
  (puzzle2 (slurp "input/2020/20-tiles.txt"))
  
  (puzzle2 (slurp "input/2020/20-example_tiles.txt")))