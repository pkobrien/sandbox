(ns gol.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


;; -- Constants ----------------------------------------------------------------

(def alpha-seed :random)  ;; usually :acorn or :random

(def alpha-state
  {:cell-radius 2
   :cell-size 8
   :cells {}
   :frame 0
   :generation 0
   :running true})

(def seeds
  {:acorn   #{[70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]}
   :foo     #{[1 1]}
   :foo-bar #{[1 1] [2 1] [1 2] [2 3] [3 4] [1 4]}})


;; -- Functions ----------------------------------------------------------------

;; (defn neighbours
;;   "Given a cell's coordinates, returns the coordinates of its neighbours."
;;   [[x y]]
;;   (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
;;     [(+ dx x) (+ dy y)]))

;; (defn step
;;   "Given a set of living cells, computes the new set of living cells."
;;   [cells]
;;   (set (for [[cell n] (frequencies (mapcat neighbours cells))
;;              :when (or (= n 3) (and (= n 2) (cells cell)))]
;;          cell)))



;; (defn neighbors [[x y]]
;;   (for [dx [-1 0 1]
;;         dy (if (zero? dx) [-1 1] [-1 0 1])]
;;     [(+ dx x) (+ dy y)]))

;; (defn changed-cells [[x y]]
;;   (for [dx [-1 0 1]
;;         dy  [-1 0 1]]
;;     [(+ dx x) (+ dy y)]))

;; (defn step [state]
;;   (let [{:keys [cells changed]} state
;;         changed-neighborhood (set (mapcat changed-cells-memo changed))
;;         changed-and-alive (s/intersection changed-neighborhood cells)
;;         changed-cells (frequencies (mapcat neighbors-memo changed-and-alive))
;;         live-counts (reduce (fn [acc loc] (assoc acc loc 0)) {} changed-and-alive)
;;         changed-cells (merge live-counts changed-cells)
;;         state (assoc state :changed #{})]
;;     (reduce (fn [acc [loc n]]
;;               (cond (and (cells loc) (or (< n 2) (> n 3)))
;;                     (-> acc
;;                         (update-in [:cells] disj loc)
;;                         (update-in [:changed] conj loc))
;;                     (and (nil? (cells loc)) (= n 3))
;;                     (-> acc
;;                         (update-in [:cells] conj loc)
;;                         (update-in [:changed] conj loc))
;;                     :else acc)) state changed-cells)))


(defn neighbors [[x y]]
  (map vector ((juxt inc      inc identity dec dec      dec identity inc) x)
              ((juxt identity inc inc      inc identity dec dec      dec) y)))

(defn random-seed [n max-x max-y offset-x offset-y]
  (set (repeatedly n #(vector (+ (rand-int max-x) offset-x)
                              (+ (rand-int max-y) offset-y)))))

(defn get-seed [seed]
  (if (= seed :random)
    (random-seed 1600 100 100 0 0)
    (seeds seed)))

;; (defn get-colors [k cells]
;;   (map #(:color %1) (keep #(get cells %1) (neighbors k))))

(defn create-cell
  ([]
   {:age 0 :color {:r (rand-int 256) :g (rand-int 256) :b (rand-int 256)}})
  ([k cells]
   ; Set newborn cell color using a blend of colors of its 3 living neighbors.
   (let [[c1 c2 c3] (map #(:color %1) (keep #(get cells %1) (neighbors k)))]
     {:age 0 :color {:r (:r c1) :g (:g c2) :b (:b c3)}})))

(defn step [cells]
  (into {} (for [[k n] (->> (keys cells) (mapcat neighbors) (frequencies))
                 :let [cell (or (get cells k) (create-cell k cells))]
                 :when (or (= n 3) (and (= n 2) (contains? cells k)))]
             [k (update-in cell [:age] inc)])))


;; -- Draw ---------------------------------------------------------------------

(defn draw-cells [state]
  (q/stroke 0)
  (let [r (:cell-radius state)
        w (:cell-size   state)]
    (doseq [[[x y] cell] (:cells state)
            :let [color (:color cell)]]
      ;(q/fill (min (:age cell) 255))
      (q/fill (:r color) (:g color) (:b color))
      (q/rect (* w x) (* w y) w w r))))

(defn draw-text [state]
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Generation: "  (:generation state)) 20 40)
  (q/text (str "Population: "  (count (:cells state))) 20 60)
  (q/text (str "Oldest Age: "  (apply max (map #(:age %1) ((fnil vec [{:age "N/A"}]) (vals (:cells state)))))) 20 80)
;  (q/text (str "Oldest Age: "  (apply (fnil max 0) (vec (map #(:age %1) (vals (:cells state)))))) 20 80)
  (q/text (str "Frame:      "  (:frame state)) 20 120)
  (q/text (str "Frame Rate: "  (q/current-frame-rate)) 20 140)
  (q/text (str "Running:    "  (:running state)) 20 160))


;; -- Setup --------------------------------------------------------------------

(defn setup-cells [seed]
  (into {} (for [k seed] [k (create-cell)])))


;; -- Quil Handlers ------------------------------------------------------------

(defn setup []
  (q/frame-rate 60)
  (->> (get-seed alpha-seed)
       (setup-cells)
       (assoc alpha-state :cells)))

(defn update [state]
  (let [state (update-in state [:frame] inc)]
    (if (:running state)
      (assoc state
        :cells (step (:cells state))
        :generation (inc (:generation state)))
      state)))

(defn draw [state]
  (q/background 120)
  (draw-cells state)
  (draw-text state))

(defn mouse-clicked [state event]
  (update-in state [:running] not))

(defn mouse-wheel [state event]
  (assoc state :cell-size (max (- (:cell-size state) event) 1)))

(q/defsketch gol
  :features [:resizable]
  :middleware [m/fun-mode]
  :size [800 800]
  :title "Conways's Game of Life"
  :setup setup
  :update update
  :draw draw
  :mouse-clicked mouse-clicked
  :mouse-wheel mouse-wheel)
