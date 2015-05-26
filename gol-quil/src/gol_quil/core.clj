(ns gol.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn get-seed []
  #{; Acorn
    [70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]
    })

(defn neighbors [[x y]]
  (map vector ((juxt inc      inc identity dec dec      dec identity inc) x)
              ((juxt identity inc inc      inc identity dec dec      dec) y)))

;; (defn get-colors [k cells]
;;   (map #(:color %1) (keep #(get cells %1) (neighbors k))))

(defn create-cell
  ([]
   {:age 0 :color {:r (rand-int 256) :g (rand-int 256) :b (rand-int 256)}})
  ([k cells]
   ; Set the color of this newborn cell using a blend of the colors of its 3 living neighbors.
   (let [[color1 color2 color3] (map #(:color %1) (keep #(get cells %1) (neighbors k)))]
     {:age 0 :color {:r (:r color1) :g (:g color2) :b (:b color3)}})))

(defn step [cells]
  (into (hash-map) (for [[k n] (frequencies (mapcat neighbors (keys cells)))
                         :when (or (= n 3) (and (= n 2) (contains? cells k)))]
                     [k (update-in (or (get cells k) (create-cell k cells)) [:age] inc)])))

(defn setup-cells [seed]
  (into (hash-map) (map #(conj [%1] (create-cell)) seed)))

(defn setup []
  (q/frame-rate 60)
  {:cell-size 8
   :cells (setup-cells (get-seed))
   :frame 0
   :generation 0
   :running true})

(defn update [state]
  (let [state (update-in state [:frame] inc)]
    (if (:running state)
      (assoc state
        :cells (step (:cells state))
        :generation (inc (:generation state)))
      state)))

(defn draw [state]
  (q/background 120)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Generation: "  (:generation state)) 20 40)
  (q/text (str "Population: "  (count (:cells state))) 20 60)
  (q/text (str "Oldest Age: "  (apply max (map #(:age %1) (vals (:cells state))))) 20 80)
  (q/text (str "Frame:      "  (:frame state)) 20 120)
  (q/text (str "Frame Rate: "  (q/current-frame-rate)) 20 140)
  (q/text (str "Running:    "  (:running state)) 20 160)
  (doseq [[[x y] cell] (:cells state)
          :let [w (:cell-size state) color (:color cell)]]
    ;(q/fill (min (:age cell) 255))
    (q/fill (:r color) (:g color) (:b color))
    (q/rect (* w x) (* w y) w w 2)))

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
