(ns quil-sketches.ergo-ca-yin-yang-fire
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


; ------------------------------------------------------------------------------
; Yin Yang Fire Cellular Automata

(defn yin-yang-fire-cell-fate
  "Returns a function that determines the fate of a cell."
  [max-states n generation neighbors-f]
  (let [index (volatile! (long -1))]
    (fn cell-fate [cell]
      (vswap! index #(inc (long %)))
      (let [cell (long cell)
            neighbors (neighbors-f @index)
            neighborhood-count (long (count neighbors))
            neighborhood-total (long (reduce + neighbors))]
        (if (< (+ (int n) (* cell neighborhood-count)) neighborhood-total)
          (mod (inc cell) max-states)
          (mod (dec cell) max-states))))))

(defn yin-yang-fire-system
  [max-states n neighborhood-f seed width height]
  (ergo/dense-fate-ca-system
    (partial yin-yang-fire-cell-fate max-states n)
    neighborhood-f seed width height))

(defn random-yin-yang-fire-system
  [max-states n neighborhood-f width height]
  (let [seed (ergo/make-seed-with-random-values (range max-states) width height)]
    (yin-yang-fire-system max-states n neighborhood-f seed width height)))


; ------------------------------------------------------------------------------
; Quil Drawing

(defonce ca-state
  (atom {:generations nil
         :new-cells nil}))

(defn draw-cell [width pixels index new-cell]
  (let [;color (q/color 300 new-cell 100)
        color (q/color new-cell 0 0)
        [x y] (ergo/wi->xy width index)
        x (* 2 (int x))
        y (* 2 (int y))
        w (* 2 (int width))]
    (aset-int pixels (+ (* y w) x) color)
    (aset-int pixels (+ (* y w) x (int 1)) color)
    (aset-int pixels (+ (* y w) x w) color)
    (aset-int pixels (+ (* y w) x w (int 1)) color)))

(defn do-fire [max-states n neighborhood-f width height fps]
  (let [title "Yin Yang Fire"
        size [(* 2 (int width)) (* 2 (int height))]
        seed (ergo/make-seed-with-random-values (range max-states) width height)
        system (yin-yang-fire-system max-states n neighborhood-f seed width height)
        setup (fn []
                #_(q/color-mode :hsb 360 (dec (int max-states)) 100)
                (q/color-mode :rgb (dec (int max-states)) 100 100)
                (q/frame-rate fps))
        update (fn [_]
                 (let [ca @ca-state]
                   (reset! ca-state {:new-cells (first (:generations ca))
                                     :generations (rest (:generations ca))})))
        draw (fn [_]
               (let [draw-cell (partial draw-cell width (q/pixels))
                     new-cells (:new-cells @ca-state)]
                 (dorun (map-indexed draw-cell new-cells))
                 (q/update-pixels)
                 (q/fill 0)
                 (q/text-size 20)
                 (q/text (str "FPS: " (int (q/current-frame-rate))) 10 30)
                 (q/text (str "F: " (q/frame-count)) 10 60)
                 ))]
    (reset! ca-state {:new-cells (first system)
                      :generations (rest system)})
    (q/sketch
      :title title
      :size size
      :setup setup
      :update update
      :draw draw
      :features [:keep-on-top]
      :middleware [m/fun-mode])))

(comment

  (do-fire 33 -5 ergo/neighborhood-9 72 72 20)

  (do-fire 15 9 ergo/neighborhood-9 288 288 20)

  (def seed (ergo/make-seed-with-random-values (range 16) 5 5))

  (def s1 (yin-yang-fire-system 16 1 ergo/neighborhood-9 seed 5 5))
  (def s2 (yin-yang-fire-system 16 2 ergo/neighborhood-9 seed 5 5))
  (def s3 (yin-yang-fire-system 16 3 ergo/neighborhood-9 seed 5 5))

  (defn prow [n] (for [row (nth (map vector s1 s2 s3) n)] (println row)))

  (->> (println row) (for [row (take 10 s1)]))
  (->> (println row) (for [row (take 10 s2)]))
  (->> (println row) (for [row (take 10 s3)]))

  )
