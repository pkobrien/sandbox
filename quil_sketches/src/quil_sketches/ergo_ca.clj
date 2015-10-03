(ns quil-sketches.ergo-ca
  (:require [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn produce-2d
  "Returns a lazy sequence of grids where a grid is a vector of vectors of
   cells."
  [seed get-cell-creator-fn]
  (letfn [(process
            [grid]
            (lazy-seq
              (when (seq grid)
                (let [cell-creator (get-cell-creator-fn grid)
                      make-cell (fn [x y cell]
                                  (cell-creator x y cell))
                      inner (fn [x column]
                              (into (empty column)
                                    (map-indexed (partial make-cell x)) column))
                      new-grid (into (empty grid)
                                     (map-indexed inner) grid)]
                  (cons new-grid (process new-grid))))))]
    (process seed)))

(defprotocol IGrid-2D
  (col-count [this])
  (row-count [this])
  )

(deftype Grid-2D [grid]
  IGrid-2D
  (col-count [_] (count grid))
  (row-count [_] (count (peek grid))))

(defn ca-2d-grid-system
  [seed grid-creator get-cell-creator-fn]
  (cons (grid-creator seed) (map grid-creator (produce-2d seed get-cell-creator-fn))))

(defrecord Cell [age color state])

(defmethod clojure.core/print-method Cell [this ^java.io.Writer writer]
  (.write writer (str "<Cell " (:age this) " " (:color this) ">")))

(defn make-cell
  ([]
   (->Cell 0 (rand-int 360) :off))
  ([age color state]
   (->Cell age color state)))

(defn make-seed [x y]
  (vec (map vec (partition y (take (* x y) (repeatedly make-cell))))))

(defn create-cell [gen grid x y cell]
  (let [neighborhood (keep #(get-in grid %) (ergo/neighborhood-4 [x y]))
        color (/ (reduce + (map :color neighborhood)) (count neighborhood))]
    (->Cell (inc (:age cell)) color (:state cell))))

(defn make-ca-system
  [seed]
  (ca-2d-grid-system
    seed
    ->Grid-2D
    (ergo/gen (fn get-cell-fn [gen grid]
                (partial create-cell gen grid)))))


; ------------------------------------------------------------------------------
; Quil Functions

(defn update-state [state]
  (merge state {:grid (first (:ca-seq state))
                :ca-seq (rest (:ca-seq state))}))

(defn draw-cells [state]
  (q/stroke 0)
  (let [radius (:cell-radius state)
        size (:cell-size state)
        top (:top-margin state)
        grid (.-grid (:grid state))]
    (doseq [[x column] (map vector (range) grid)
            [y cell] (map vector (range) column)]
      (q/fill (:color cell) 100 100)
      (q/rect (* size x) (+ (* size y) top) size size radius))))

(defn draw-text [state]
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "FPS: " (int (q/current-frame-rate))) 10 30))

(defn draw-state [state]
  (q/background 220)
  (when (:grid state)
    (draw-cells state)
    (draw-text state)))

(defn setup [setup-opts state]
  (q/frame-rate (:frame-rate setup-opts))
  (apply q/color-mode (:color-mode setup-opts))
  state)

(defn sketch [setup-opts state]
  (q/sketch
    :title "Cellular Automata"
    :size [720 460]
    :setup (partial setup setup-opts state)
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

(defn setup-opts []
  {:frame-rate 2
   :color-mode [:hsb 360 100 100]})

(defn get-state []
  {:ca-seq (make-ca-system (make-seed 72 36))
   :cell-radius 2
   :cell-size 10
   :frame 0
   :generation 0
   :grid nil
   :running true
   :top-margin 100})

(comment

  (sketch (setup-opts) (get-state))

  )
