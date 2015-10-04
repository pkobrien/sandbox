(ns quil-sketches.ergo-ca
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))


(def neighborhood-4-x (juxt inc identity dec identity))
(def neighborhood-4-y (juxt identity inc identity dec))

(defn neighborhood-4-tx [tx]
  (juxt (comp tx inc) identity (comp tx dec) identity))

(defn neighborhood-4-ty [ty]
  (juxt identity (comp ty inc) identity (comp ty dec)))

(defn neighborhood-4
  "Returns a vector of the 4 [x y] pairs of a von Neumann neighborhood."
  ([[x y]]
   (map vector (neighborhood-4-x x) (neighborhood-4-y y)))
  ([tx ty [x y]]
   (map vector ((neighborhood-4-tx tx) x) ((neighborhood-4-ty ty) y))))

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(defn neighborhood-8-tx [tx]
  (let [t-inc (comp tx inc)
        t-dec (comp tx dec)]
    (juxt t-inc t-inc identity t-dec t-dec t-dec identity t-inc)))

(defn neighborhood-8-ty [ty]
  (let [t-inc (comp ty inc)
        t-dec (comp ty dec)]
    (juxt identity t-inc t-inc t-inc identity t-dec t-dec t-dec)))

(defn neighborhood-8
  ([[x y]]
   (map vector (neighborhood-8-x x) (neighborhood-8-y y)))
  ([tx ty [x y]]
   (map vector ((neighborhood-8-tx tx) x) ((neighborhood-8-ty ty) y))))


(defn produce-2d
  "Returns a lazy sequence of grids where a grid is a vector of vectors of
   cells."
  [seed get-cell-creator-fn]
  (letfn [(process
            [grid]
            (lazy-seq
              (when (seq grid)
                (let [cell-creator (get-cell-creator-fn grid)
                      create-cell (fn [x y cell]
                                    (cell-creator x y cell))
                      inner (fn [x column]
                              (into (empty column)
                                    (map-indexed (partial create-cell x))
                                    column))
                      new-grid (into (empty grid)
                                     (map-indexed inner)
                                     grid)]
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

(defrecord Cell-N8 [n8 age color state])

(defmethod clojure.core/print-method Cell [this ^java.io.Writer writer]
  (.write writer (str "<Cell " (:age this) " " (:color this) ">")))

(defn make-cell
  ([]
   (->Cell 0 (rand-int 360) :off))
  ([age color state]
   (->Cell age color state)))

(defn make-cell-n8
  ([n8]
   (->Cell-N8 n8 0 (rand-int 360) :off))
  ([n8 age color state]
   (->Cell-N8 n8 age color state)))

(defn make-seed [w h]
  (vec (map vec (partition h (take (* w h) (repeatedly make-cell))))))

(defn make-seed-f [w h f]
  (vec (map vec (partition h (take (* w h) (f w h))))))

(defn td [extent]
  (fn [i]
    (mod i extent)))

(defn make-seed-cells-n8 [w h]
  (let [tx (td w)
        ty (td h)]
    (for [x (range w)
          y (range h)]
      (make-cell-n8 (vec (neighborhood-8 tx ty [x y]))))))

(defn create-cell [gen w h grid tx ty x y cell]
  (let [neighbors (map #(get-in grid %) (neighborhood-4 tx ty [x y]))
        avg-color (/ (reduce + (map :color neighbors)) (count neighbors))]
    (->Cell (inc (:age cell)) avg-color (:state cell))))

(defn create-aged-cell [gen w h grid tx ty x y cell]
  (->Cell (inc (:age cell)) (:color cell) (:state cell)))

(defn create-colored-4-cell [gen w h grid tx ty x y cell]
  (let [neighbors (map #(get-in grid %) (neighborhood-4 tx ty [x y]))
        avg-color (/ (reduce + (map :color neighbors)) (count neighbors))]
    (->Cell (inc (:age cell)) avg-color (:state cell))))

(defn create-colored-8-cell
  "Returns a cell whose color moves towards the average of its neighbors."
  [gen w h grid tx ty x y cell]
  (let [neighbors (map #(get-in grid %) (neighborhood-8 tx ty [x y]))
        avg-color (/ (reduce + (map :color neighbors)) (count neighbors))]
    (->Cell (inc (:age cell)) (/ (+ (:color cell) avg-color) 2) (:state cell))))

#_(defn create-colored-8-cell
  "Returns a cell whose color the average of its own and a random neighbor."
  [gen w h grid tx ty x y cell]
  (let [neighbors (map #(get-in grid %) (neighborhood-8 tx ty [x y]))
        random-color (rand-nth (map :color neighbors))]
    (->Cell (inc (:age cell)) (/ (+ (:color cell) random-color) 2) (:state cell))))

(defn create-colored-n8-cell
  "Returns a cell whose color moves towards the average of its neighbors."
  [gen w h grid x y cell]
  (let [neighbors (map #(get-in grid %) (:n8 cell))
        avg-color (/ (reduce + (map :color neighbors)) (count neighbors))]
    (->Cell-N8 (:n8 cell) (inc (:age cell)) (/ (+ (:color cell) avg-color) 2) (:state cell))))


(defn make-ca-system
  [w h]
  (let [tx (fn toroidal-x [x] (mod x w))
        ty (fn toroidal-y [y] (mod y h))]
    (ca-2d-grid-system
      (make-seed w h)
      ->Grid-2D
      (ergo/gen (fn get-cell-fn [gen grid]
                  (partial create-cell gen w h grid tx ty))))))

(defn make-ca-system-aged
  [w h]
  (let [tx (fn toroidal-x [x] (mod x w))
        ty (fn toroidal-y [y] (mod y h))]
    (ca-2d-grid-system
      (make-seed w h)
      ->Grid-2D
      (ergo/gen (fn get-cell-fn [gen grid]
                  (partial create-aged-cell gen w h grid tx ty))))))

(defn make-ca-system-colored-4
  [w h]
  (let [tx (fn toroidal-x [x] (mod x w))
        ty (fn toroidal-y [y] (mod y h))]
    (ca-2d-grid-system
      (make-seed w h)
      ->Grid-2D
      (ergo/gen (fn get-cell-fn [gen grid]
                  (partial create-colored-4-cell gen w h grid tx ty))))))

(defn make-ca-system-colored-8
  [w h]
  (let [tx (fn toroidal-x [x] (mod x w))
        ty (fn toroidal-y [y] (mod y h))]
    (ca-2d-grid-system
      (make-seed w h)
      ->Grid-2D
      (ergo/gen (fn get-cell-fn [gen grid]
                  (partial create-colored-8-cell gen w h grid tx ty))))))

(defn make-ca-system-colored-n8
  [w h]
  (ca-2d-grid-system
    (make-seed-f w h make-seed-cells-n8)
    ->Grid-2D
    (ergo/gen (fn get-cell-fn [gen grid]
                (partial create-colored-n8-cell gen w h grid)))))


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Unoptimized color average of 4 neighbors: 6.00 sec
    #(-> (make-ca-system 72 36) (nth 60)))

  (bench ; Make seeds: 222 ms
    #(dorun 60 (repeatedly (fn [] (make-seed 72 36)))))

  (bench ; Make seeds n8: 2.69 sec
    #(dorun 60 (repeatedly (fn [] (make-seed-f 72 36 make-seed-cells-n8)))))

  (bench ; Only increments age: 364 ms
    #(-> (make-ca-system-aged 72 36) (nth 60)))

  (bench ; Color average of 4 neighbors: 6.00 sec
    #(-> (make-ca-system-colored-4 72 36) (nth 60)))

  (bench ; Color average of 8 neighbors: 20.5 sec
    #(-> (make-ca-system-colored-8 72 36) (nth 60)))

  (bench ; Color average of n8 neighbors: 17.48 sec
    #(-> (make-ca-system-colored-n8 72 36) (nth 60)))

  )


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

#_(defn draw-cells [state]
  (q/stroke 0)
  (let [radius (:cell-radius state)
        size (:cell-size state)
        top (:top-margin state)
        grid (.-grid (:grid state))
        draw (fn [x y cell]
               (q/fill (:color cell) 100 100)
               (q/rect (* size x) (+ (* size y) top) size size radius))
        inner (fn [x column]
                (dorun (map-indexed (fn [y cell] (draw x y cell)) column)))]
    (dorun (map-indexed inner grid))))

(defn draw-text [state]
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Frame: " (q/frame-count)) 10 30)
  (q/text (str "FPS: " (int (q/current-frame-rate))) 10 60))

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

(defn setup-opts [fps]
  {:frame-rate fps
   :color-mode [:hsb 360 100 100]})

(defn get-state [system]
  {:ca-seq system
   :cell-radius 2
   :cell-size 10
   :frame 0
   :generation 0
   :grid nil
   :running true
   :top-margin 100})

(comment

  (sketch (setup-opts 20) (get-state (make-ca-system 72 36)))

  (sketch (setup-opts 20) (get-state (make-ca-system-aged 72 36)))

  (sketch (setup-opts 20) (get-state (make-ca-system-colored-4 72 36)))

  (sketch (setup-opts 1) (get-state (make-ca-system-colored-8 72 36)))

  (sketch (setup-opts 10) (get-state (make-ca-system-colored-n8 72 36)))

  )
