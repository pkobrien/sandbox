(ns quil-sketches.ergo-ca-2
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn make-seed-2
  "Returns a vector of values based on calling (f x y)."
  [w h f]
  (vec (for [y (range h)
             x (range w)]
         (f x y))))

(defn make-seed-for-pattern
  [live-cell dead-cell w h pattern]
  (let [make-cell (fn [x y]
                    (if (pattern [x y]) live-cell dead-cell))]
    (make-seed-2 w h make-cell)))

(defn make-seed-for-acorn
  [live-cell dead-cell w h]
  (make-seed-for-pattern
    live-cell dead-cell w h #{[0 2] [1 0] [1 2] [3 1] [4 2] [5 2] [6 2]}))


(defn make-seed
  "Returns a vector of values based on calling f, which should return a lazy
   infinite sequence."
  [w h f]
  (vec (take (* (long w) (long h)) (f))))

(defn make-seed-for-age [w h]
  (make-seed w h #(repeat (long 0))))

(def random-color (partial rand-int 360))

(defn make-seed-for-random-color [w h]
  (make-seed w h (fn [x y] (random-color))))

(defn cell-color-blend
  "Returns a color influenced by the colors of neighboring cells."
  [weight [cell neighbors]]
  (let [weight (int weight)
        color (int cell)
        n (int (count neighbors))
        color-total (+ (* weight color) (int (reduce + neighbors)))
        color-count (+ weight n)]
    (quot color-total color-count)))

(defn cell-color-blending
  "Returns a cell color-blending transducer."
  [weight]
  (map (partial cell-color-blend weight)))

(defn example-color-blend-ca-system [w h]
  (let [seed (make-seed-for-random-color w h)
        neighbors-index (ergo/make-neighbors-lookup ergo/neighborhood-8 w h)
        contextualizing (partial ergo/contextualizing neighbors-index)
        coloring (cell-color-blending 200)]
    (ergo/dense-ca-system
      seed
      (ergo/gen (fn [generation word]
                  (comp (contextualizing word)
                        coloring))))))


(def random-life (partial rand-int 2))

(defn make-seed-for-random-life [w h]
  (make-seed w h #(repeatedly random-life)))


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Make seeds for age: 46 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-age 72 36)))))

  (bench ; Make seeds for color: 101 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-random-color 72 36)))))

  (bench ; Make index for n8: 37 ms
    #(ergo/make-neighbors-lookup ergo/neighborhood-8 72 36))

  (bench ; Color Blend - 60 gens: 635 ms
    #(-> (example-color-blend-ca-system 72 36) (nth 59)))

  (bench ; Color Blend - 600 gens: 6 sec
    #(-> (example-color-blend-ca-system 72 36) (nth 599)))

  (bench ; Conway GOL - Random Seed - [:alive :dead] - 60 gens: 570 ms
    #(-> (ergo/dense-life-ca-system
           #{2 3} #{3} ergo/neighborhood-8 :alive :dead
           (ergo/make-seed-for-random-cell-value [:alive :dead] 72 36) 72 36)
         (nth 59)))


  (bench ; Conway GOL - Acorn Seed - [:alive :dead] - 60 gens: 480 ms
    #(-> (ergo/dense-life-ca-system
           #{2 3} #{3} ergo/neighborhood-8 :alive :dead
           (make-seed-for-acorn :alive :dead 72 36) 72 36)
         (nth 59)))

  (bench ; Conway GOL - Acorn Seed - [1 0] - 60 gens: 480 ms
    #(-> (ergo/dense-life-ca-system
           #{2 3} #{3} ergo/neighborhood-8 1 0
           (make-seed-for-acorn 1 0 72 36) 72 36)
         (nth 59)))

  (bench ; Conway GOL - Acorn Seed - [1 0] - 100 gens: 750 ms
    #(-> (ergo/dense-life-ca-system
           #{2 3} #{3} ergo/neighborhood-8 1 0
           (make-seed-for-acorn 1 0 72 36) 72 36)
         (nth 99)))

  )


; ------------------------------------------------------------------------------
; Quil Functions

(defn setup [setup-opts state]
  (q/frame-rate (:frame-rate setup-opts))
  (apply q/color-mode (:color-mode setup-opts))
  state)

(defn update-state [state]
  (merge state {:cells (first (:ca-seq state))
                :ca-seq (rest (:ca-seq state))}))

(defn sketch [setup-opts draw-f state]
  (q/sketch
    :title "Cellular Automata"
    :size [720 460]
    :setup (partial setup setup-opts state)
    :update update-state
    :draw draw-f
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


; ------------------------------------------------------------------------------
; Quil Color Cells

(defn draw-color-cells [state]
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

(defn draw-color-text [state]
  (q/fill 0)
  (q/text-size 20)
  (q/text (str "Frame: " (q/frame-count)) 10 30)
  (q/text (str "FPS: " (int (q/current-frame-rate))) 10 60))

(defn draw-color-state [state]
  (q/background 220)
  (when (:cells state)
    (draw-color-cells state)
    (draw-color-text state)))

(comment

  (sketch (setup-opts 20)
          draw-color-state
          (get-state (example-color-blend-ca-system 72 36)))

  )


; ------------------------------------------------------------------------------
; Quil Life-Like Cells

;(defn life-sketch [setup-opts draw-f state]
;  (q/sketch
;    :title "Cellular Automata"
;    :size [1080 640]
;    :setup (partial setup setup-opts state)
;    :update update-state
;    :draw draw-f
;    :features [:keep-on-top]
;    :middleware [m/fun-mode]))
;
;(defn life-setup-opts [fps]
;  {:frame-rate fps
;   :color-mode [:hsb 360 100 100]})
;
;(defn life-get-state [system]
;  {:ca-seq system
;   :ca-height 108
;   :cells nil
;   :cell-radius 1
;   :cell-size 5
;   :frame 0
;   :generation 0
;   :running true
;   :top-margin 100})
;
;(defn draw-life-cells [state]
;  (q/stroke 0)
;  (let [cells (:cells state)
;        radius (:cell-radius state)
;        size (int (:cell-size state))
;        top (int (:top-margin state))
;        height (:ca-height state)
;        i->xy (partial ergo/hi->xy height)
;        hue 200]
;    (doseq [[i cell] (map vector (range) cells)]
;      (when (= :alive cell)
;        (q/fill hue 100 100)
;        (let [[x y] (i->xy i)
;              x (int x)
;              y (int y)]
;          (q/rect (* size x) (+ (* size y) top) size size radius))))))
;
;(defn draw-life-text [state]
;  (q/fill 0)
;  (q/text-size 20)
;  (q/text (str "Frame: " (q/frame-count)) 10 30)
;  (q/text (str "FPS: " (int (q/current-frame-rate))) 10 60))
;
;(defn draw-life-state [state]
;  (q/background 220)
;  (when (:cells state)
;    (draw-life-cells state)
;    (draw-life-text state)))
;
;(comment
;
;  (life-sketch
;    (life-setup-opts 60)
;    draw-life-state
;    (life-get-state
;      (ergo/dense-life-ca-system
;        #{2 3} #{3} ergo/neighborhood-8 :alive :dead
;        (make-seed-for-acorn :alive :dead 216 108) 216 108)
;      #_(old-life-ca-system #{2 3} #{3}
;                              ergo/neighborhood-8
;                              144 72)))
;
;  )


; ------------------------------------------------------------------------------
; Quil Life-Like Cells using Pixels

(defn life-sketch [setup-opts draw-f state]
  (q/sketch
    :title "Cellular Automata"
    :size [(:ca-width state) (:ca-height state)]
    :setup (partial setup setup-opts state)
    :update update-state
    :draw draw-f
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

(defn life-setup-opts [fps]
  {:frame-rate fps
   :color-mode [:hsb 360 100 100]})

(defn life-get-state [system]
  {:ca-seq system
   :ca-width 360
   :ca-height 360
   :cells nil})

(defn draw-life-cells [state]
  (let [pixels (q/pixels)
        cells (:cells state)
        alive-color (q/color 300 100 100)
        dead-color (q/color 100 100 100)]
    (doseq [[i cell] (map vector (range) cells)]
      (if (= :alive cell)
        (aset-int pixels i alive-color)
        (aset-int pixels i dead-color)))
    (q/update-pixels)))

(defn draw-life-state [state]
  (when (:cells state)
    (draw-life-cells state)))

(comment

  (life-sketch
    (life-setup-opts 60)
    draw-life-state
    (life-get-state
      (ergo/dense-life-ca-system
        #{2 3} #{3} ergo/neighborhood-8 :alive :dead
        (make-seed-for-acorn :alive :dead 360 360) 360 360)))

  )
