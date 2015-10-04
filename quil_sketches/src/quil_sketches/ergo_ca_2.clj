(ns quil-sketches.ergo-ca-2
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))

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

(defn produce
  "Returns a lazy sequence of colls from a recursive, axiomatic, transducible
   process."
  [seed prep-f get-xf]
  (letfn [(process
            [coll]
            (lazy-seq
              (when (seq coll)
                (let [new-coll (into (empty coll) (get-xf coll) (prep-f coll))]
                  (cons new-coll (process new-coll))))))]
    (process seed)))

(defn dense-ca-system
  [seed get-xf]
  (cons seed (produce seed identity get-xf)))

(defn make-seed-f [w h f]
  (vec (take (* w h) (f))))

(defn make-seed-for-age [w h]
  (make-seed-f w h #(repeat 0)))

(def random-color (partial rand-int 360))

(defn make-seed-for-color [w h]
  (make-seed-f w h #(repeatedly random-color)))

(defn td [extent]
  (fn [i]
    (mod i extent)))

(defn ->xy [height i]
  [(quot i height) (mod i height)])

;(defn neighbors-8 [word tx ty width i]
;  (map (fn [[x y]] (nth word (+ x (* y width))))
;       (neighborhood-8 tx ty (->xy width i))))

;(defn color [generation word tx ty width]
;  (let [index (volatile! (long -1))]
;    (fn context-sensitive-color [cell-color]
;      (vswap! index #(inc (long %)))
;      (let [neighbors (neighbors-8 word tx ty width @index)
;            avg-color (int (/ (reduce + neighbors) (count neighbors)))]
;        (int (/ (+ cell-color avg-color) 2))))))

(defn color [generation word n8]
  (let [index (volatile! (long -1))]
    (fn context-sensitive-color [cell-color]
      (vswap! index #(inc (long %)))
      (let [neighbors (map #(nth word %) (nth n8 @index))
            avg-color (int (/ (reduce + neighbors) (count neighbors)))]
        (int (/ (+ cell-color avg-color) 2))))))

(defn coloring [generation word n8]
  (map (color generation word n8)))

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation number and an additional context argument."
  [f]
  (let [generation (volatile! (long -1))]
    (fn
      [data]
      (vswap! generation #(inc (long %)))
      (f @generation data))))

(defn make-index-n8 [w h]
  (let [tx (td w)
        ty (td h)
        t8 (partial neighborhood-8 tx ty)]
    (into [] (for [x (range w)
                   y (range h)]
               (vec (map (fn [[x y]] (+ (* x h) y)) (t8 [x y])))))))

(defn ca-for-color [w h]
  (let [n8 (make-index-n8 w h)]
    (dense-ca-system (make-seed-for-color w h)
                     (gen (fn [generation word] (coloring generation word n8))))))


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Make seeds for age: 46 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-age 72 36)))))

  (bench ; Make seeds for color: 101 ms
    #(dorun 60 (repeatedly (fn [] (make-seed-for-color 72 36)))))

  (bench ; Make index for n8: 2.6 sec
    #(dorun 60 (repeatedly (fn [] (make-index-n8 72 36)))))

  (bench ; CA for color 1: 44 ms
    #(-> (ca-for-color 72 36) (nth 0)))

  (bench ; CA for color 2: 56 ms
    #(-> (ca-for-color 72 36) (nth 1)))

  (bench ; CA for color 60: 777 ms
    #(-> (ca-for-color 72 36) (nth 59)))

  (bench ; CA for color 600: 6.45 sec
    #(-> (ca-for-color 72 36) (nth 599)))

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
        size (:cell-size state)
        top (:top-margin state)
        height (:ca-height state)
        i->xy (partial ->xy height)]
    (doseq [[i color] (map vector (range) cells)]
      (q/fill color 100 100)
      (let [[x y] (i->xy i)]
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

  (sketch (setup-opts 20) (get-state (ca-for-color 72 36)))

  )
