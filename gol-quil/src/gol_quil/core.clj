(ns gol.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn seed []
  #{; Acorn
    [70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]
    })

(defn neighbors [[x y]]
  (map vector ((juxt inc      inc identity dec dec      dec identity inc) x)
              ((juxt identity inc inc      inc identity dec dec      dec) y)))

(defn step [cells]
  (into (hash-map) (for [[k n] (frequencies (mapcat neighbors (keys cells)))
                         :when (or (= n 3) (and (= n 2) (get cells k)))]
                     [k (inc (get cells k 0))])))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:cell-size 8
   :cells (into (hash-map) (map #(conj [%1] 0) (seed)))
   :generation 0})

(defn update [state]
  (assoc state
    :cells (step (:cells state))
    :generation (inc (:generation state))))

(defn draw [state]
  (q/background 120)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Generation: "  (:generation state)) 20 40)
  (q/text (str "Population: "  (count (:cells state))) 20 60)
  (q/text (str "Oldest Age: "  (apply max (vals (:cells state)))) 20 80)
  ; (q/text (str "Frequencies: " (frequencies (vals (:cells state)))) 20 100)
  (doseq [[[x y] age] (:cells state)
          :let [w (:cell-size state)]]
    (q/fill (min age 255))
    (q/rect (* w x) (* w y) w w 2)))

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
  :mouse-wheel mouse-wheel)
