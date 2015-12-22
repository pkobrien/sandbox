(ns quil-sketches.ergo-ca-benchmarking
  (:require [criterium.core :as cr]
            [ion.ergo.core :as ergo]
            [quil.core :as q]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn neighbours-count [above current below i w]
  (let [j (mod (inc (long i)) w)
        k (mod (dec (long i)) w)
        s #(if (= (%1 %2) :on) 1 0)]
    (+ (+ (+ (long (s above j)) (long (s above i)))
          (+ (long (s above k)) (long (s current j))))
       (+ (+ (long (s current k)) (long (s below j)))
          (+ (long (s below i)) (long (s below k)))))))

(defn step1 [[above current below]]
  (let [w (count current)]
    (loop [i (dec w), row (transient current)]
      (if (neg? i)
        (persistent! row)
        (let [c (current i)]
          (cond
            (= c :on)
            (recur (dec i) (assoc! row i :dying))
            (= c :dying)
            (recur (dec i) (assoc! row i :off))
            (= 2 (neighbours-count above current below i w))
            (recur (dec i) (assoc! row i :on))
            :else
            (recur (dec i) row)))))))

(defn step [board]
  (vec (map step1
            (partition 3 1 (concat [(peek board)] board [(first board)])))))

(defn rand-board [w h]
  (vec (map vec (partition w (take (* w h) (repeatedly #(if (zero? (rand-int 3)) :on :off)))))))


(defn neighborhood
  "Returns a function that returns a lazy sequence of [x y] pairs of a
   neighborhood for a given [x y] vector."
  [nf-x nf-y]
  (fn [[x y]]
    (map vector (nf-x x) (nf-y y))))

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(def neighborhood-8 (neighborhood neighborhood-8-x neighborhood-8-y))

(defn row-whxy->i
  "Returns the linear index position in a row-major-ordered 1D array for the
   given [x y] coordinates with toroidal adjustments applied to the x and y
   values based on the grid width and grid height."
  [w h [x y]]
  (let [w (long w)
        h (long h)
        x (long (mod x w))
        y (long (mod y h))
        i (+ x (* y w))]
    i))

(defn make-neighborhood-lookup
  "Returns a row-major order vector of vectors of neighborhood positions for
   each cell position for a given width and height."
  [neighborhood-f width height]
  (let [xy->i (partial row-whxy->i width height)]
    (into [] (for [y (range height)
                   x (range width)]
               (mapv xy->i (neighborhood-f [x y]))))))

(defn get-neighbors
  "Returns a function that returns a lazy sequence of neighbors of the cell at
   index in word."
  [neighborhood-lookup word]
  (fn [index] (map word (neighborhood-lookup index))))

(defn sum-neighbors
  "Returns a function that returns a lazy sequence of neighbors of the cell at
   index in word."
  [neighborhood-lookup word]
  (let [total (volatile! (long 0))
        sum (fn [i] (vswap! total #(+ (long %1) (long %2)) (word i)))]
    (fn [index]
      (vreset! total (long 0))
      (dorun (map sum (neighborhood-lookup index)))
      @total)))


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; Sum of all neighbors: 730 ms
    #(let [lookup (make-neighborhood-lookup ergo/neighborhood-8 200 200)
           sum (fn [word indices] (reduce + (map word indices)))]
      (let [word (ergo/make-seed-with-random-values [0 1] 200 200)]
        (dorun (into [] (map (partial sum word)) lookup)))))

  (bench ; Sum of all neighbors: 730 ms
    #(let [lookup (make-neighborhood-lookup ergo/neighborhood-8 200 200)
           word (ergo/make-seed-with-random-values [0 1] 200 200)
           neighs (into [] (for [indices lookup] (mapv word indices)))]
      (dorun (map (fn [i] (reduce + (neighs i))) (range (count word))))))

  (bench ; Sum of all neighbors: 730 ms
    #(let [lookup (make-neighborhood-lookup ergo/neighborhood-8 200 200)
           word (ergo/make-seed-with-random-values [0 1] 200 200)
           get-n (get-neighbors lookup word)]
      (dorun (map (fn [i] (reduce + (get-n i))) (range (count word))))))



  (do  (let [b (rand-board 80 80)] (time ((apply comp (repeat 1000 step)) b))) nil)

  (do (let [b (rand-board 200 200)] (time ((apply comp (repeat 100 step)) b))) nil)

  (let [lookup (make-neighborhood-lookup ergo/neighborhood-8 200 200)
        word (ergo/make-seed-with-random-values [0 1] 200 200)
        get-n (get-neighbors lookup word)]
    (bench ; Sum of all neighbors: 100 ms
      #(dorun (map (fn [i] (reduce + (get-n i))) (range (* 200 200))))))

  (bench ; Make lookup for n8: 580 ms
    #(make-neighborhood-lookup neighborhood-8 200 200))

  )
