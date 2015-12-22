(ns quil-sketches.ergo-ca-brians-brain
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


; ------------------------------------------------------------------------------
; Brian's Brain Cellular Automata

(defn cell-state-candidate-set
  "Returns a function that determines the fate of a cell."
  [on-cell off-cell dying-cell birth? on-count-f neighbors-f candidate?]
  (let [index (volatile! (long -1))]
    (fn cell-fate [cell]
      (vswap! index #(inc (long %)))
      (if-not (candidate? @index)
        off-cell
        (cond
          (= cell on-cell) dying-cell
          (= cell dying-cell) off-cell
          (= cell off-cell) (let [neighbors (neighbors-f @index)
                                  on-neighbor-count (reduce on-count-f 0 neighbors)]
                              (if (birth? on-neighbor-count) on-cell off-cell)))))))

(defn dense-candidate-set-ca-system
  [birth? neighborhood-f on-cell off-cell dying-cell seed w h]
  (let [neighbors-lookup (ergo/make-neighborhood-lookup neighborhood-f w h)
        on-count-f (ergo/cell-counter on-cell)
        life (partial cell-state-candidate-set on-cell off-cell dying-cell
                      birth? on-count-f)]
    (ergo/dense-ca-system
      seed
      (ergo/gen (fn [generation word]
             (let [neighbors-f (ergo/get-neighbors neighbors-lookup word)
                   candidate? (ergo/get-candidates off-cell neighbors-lookup word)]
               (map (life neighbors-f candidate?))))))))


; ------------------------------------------------------------------------------
; Quil Drawing

(defn get-draw-cell-f
  [w on-color dying-color off-color]
  (fn [pixels old-cells ^long index new-cell]
    (when (not= new-cell (old-cells index))
      (let [color (case new-cell :on on-color :dying dying-color :off off-color)
            [x y] (ergo/wi->xy w index)
            x (* 2 (int x))
            y (* 2 (int y))
            w (* 2 (int w))]
        (aset-int pixels (+ (* y w) x) color)
        (aset-int pixels (+ (* y w) x (int 1)) color)
        (aset-int pixels (+ (* y w) x w) color)
        (aset-int pixels (+ (* y w) x w (int 1)) color)))))

(defn setup-opts [fps]
  {:frame-rate fps
   :color-mode [:hsb 360 100 100]})

(defn setup [w setup-opts state]
  (q/frame-rate (:frame-rate setup-opts))
  (apply q/color-mode (:color-mode setup-opts))
  (let [on-color (q/color 300 100 100)
        dying-color (q/color 200 100 100)
        off-color (q/color 100 100 100)]
    (merge state
           {:draw-cell (get-draw-cell-f w on-color dying-color off-color)})))

(defn sketch [w h setup-opts update-f draw-f state]
  (q/sketch
    :title "Brian's Brains"
    :size [(* 2 (int w)) (* 2 (int h))]
    :setup (partial setup w setup-opts state)
    :update update-f
    :draw draw-f
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

(defonce ca-state
  (atom {:generations nil
         :new-cells nil
         :old-cells nil}))

(defn draw-state [state]
  ;(q/background 220)
  (let [pixels (q/pixels)
        new-cells (:new-cells @ca-state)
        old-cells (:old-cells @ca-state)
        draw-cell (partial (:draw-cell state) pixels old-cells)]
    (dorun (map-indexed draw-cell new-cells))
    (q/update-pixels)
    ;(q/fill 0)
    ;(q/text-size 20)
    ;(q/text (str "FPS: " (int (q/current-frame-rate))) 10 30)
    ;(q/text (str "Frame: " (q/frame-count)) 100 30)
    )
  )

(defn do-brain-acorn [^long w ^long h fps]
  (let [system (dense-candidate-set-ca-system
                 #{2} ergo/neighborhood-8 :on :off :dying
                 (ergo/make-seed-for-acorn :on :off w h) w h)
        update (fn [state]
                 (let [ca @ca-state]
                   (reset! ca-state {:old-cells (:new-cells ca)
                                     :new-cells (first (:generations ca))
                                     :generations (rest (:generations ca))}))
                 state)]
    (reset! ca-state {:old-cells (vec (repeat (* w h) :dead))
                      :new-cells (first system)
                      :generations (rest system)})
    (sketch w h (setup-opts fps) update draw-state {})))

(defn do-brain-random [^long w ^long h fps]
  (let [system (dense-candidate-set-ca-system
                 #{2} ergo/neighborhood-8 :on :off :dying
                 (ergo/make-seed-with-random-values [:on :off] w h) w h)
        update (fn [state]
                 (let [ca @ca-state]
                   (reset! ca-state {:old-cells (:new-cells ca)
                                     :new-cells (first (:generations ca))
                                     :generations (rest (:generations ca))}))
                 state)]
    (reset! ca-state {:old-cells (vec (repeat (* w h) :dead))
                      :new-cells (first system)
                      :generations (rest system)})
    (sketch w h (setup-opts fps) update draw-state {})))

(comment

  (do-brain-acorn 360 180 60)

  (do-brain-random 360 180 2)

  )


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Brian's Brain - Random 200x200 - 100 gens: 5.35 sec
    #(-> (dense-candidate-set-ca-system
           #{2} ergo/neighborhood-8 :on :off :dying
           (ergo/make-seed-with-random-values [:on :off] 200 200) 200 200)
         (nth 99)))

  )
