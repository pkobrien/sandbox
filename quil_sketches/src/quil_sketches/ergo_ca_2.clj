(ns quil-sketches.ergo-ca-2
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn make-seed
  "Returns a vector of values based on calling f, which should return a lazy
   infinite sequence."
  [w h f]
  (vec (take (* (long w) (long h)) (f))))

(defn make-seed-for-age [w h]
  (make-seed w h #(repeat (long 0))))

(def random-color (partial rand-int 360))

(defn make-seed-for-color [w h]
  (make-seed w h #(repeatedly random-color)))

(defn cell-color
  "Returns a color influenced by the colors of neighboring cells."
  [[color neighbors]]
  (let [color (int color)
        n (int (count neighbors))
        color-total (+ (* 200 color) (int (reduce + neighbors)))
        color-count (+ 200 n)
        color-average (quot color-total color-count)]
    color-average))

(defn cell-coloring
  "Returns a cell-coloring transducer."
  []
  (map cell-color))

(defn example-color-ca-system [w h]
  (let [seed (make-seed w h #(repeatedly random-color))
        neighbors-index (ergo/make-neighbors-index w h ergo/neighborhood-8)]
    (ergo/dense-ca-system
      seed
      (ergo/gen (fn [generation word]
                  (comp (ergo/contextualizing word neighbors-index)
                        (cell-coloring)))))))


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Make seeds for age: 46 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-age 72 36)))))

  (bench ; Make seeds for color: 101 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-color 72 36)))))

  (bench ; Make index for n8: 37 ms
    #(ergo/make-neighbors-index 72 36 ergo/neighborhood-8))

  (bench ; CA for color 1: 37 ms
    #(-> (example-color-ca-system 72 36) (nth 0)))

  (bench ; CA for color 2: 50 ms
    #(-> (example-color-ca-system 72 36) (nth 1)))

  (bench ; CA for color 60: 635 ms
    #(-> (example-color-ca-system 72 36) (nth 59)))

  (bench ; CA for color 600: 6 sec
    #(-> (example-color-ca-system 72 36) (nth 599)))

  )


; ------------------------------------------------------------------------------
; Quil Functions

(defn update-state [state]
  (merge state {:cells (first (:ca-seq state))
                :ca-seq (rest (:ca-seq state))}))

(defn draw-cells [state]
  (q/stroke 0)
  (let [cells (:cells state)
        radius (:cell-radius state)
        size (int (:cell-size state))
        top (int (:top-margin state))
        height (:ca-height state)
        i->xy (partial ergo/hi->xy height)]
    (doseq [[i color] (map vector (range) cells)]
      (q/fill color 100 100)
      (let [[x y] (i->xy i)
            x (int x)
            y (int y)]
        (q/rect (* size x) (+ (* size y) top) size size radius)))))

(defn draw-text [state]
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Frame: " (q/frame-count)) 10 30)
  (q/text (str "FPS: " (int (q/current-frame-rate))) 10 60))

(defn draw-state [state]
  (q/background 220)
  (when (:cells state)
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
   :ca-height 36
   :cells nil
   :cell-radius 2
   :cell-size 10
   :frame 0
   :generation 0
   :running true
   :top-margin 100})

(comment

  (sketch (setup-opts 20) (get-state (example-color-ca-system 72 36)))

  )
