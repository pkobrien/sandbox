(ns cad.pandora
  (:require
    [scad-clj.scad :refer [write-scad]]
    [scad-clj.model :refer [cube cylinder polyhedron sphere
                            difference intersection union
                            hull minkowski rotate scale translate
                            with-center with-fa with-fn with-fs]]))

(defn ball [radius faces]
  (->> (sphere radius)
       (with-fn faces)
       (translate [radius radius radius])))

(defn block [size]
  (cube size size size :center false))

(defn position [units size n]
  (let [offset (/ (- size units) 2)]
    (- (* n units) offset)))

(defn grid-3d [nx ny nz]
  (for [z (range nz)
        y (range ny)
        x (range nx)]
    [x y z]))

(defn make-ball [units radius i [x y z]]
  (let [faces (+ 5 i)
        size (* 2 radius)
        pos (partial position units size)]
    (->> (ball radius faces)
         (translate [(pos x) (pos y) (pos z)]))))

(defn make-block [x y z units size]
  (let [pos (partial position units size)]
    (->> (block size)
         (translate [(pos x) (pos y) (pos z)]))))

(defn ball-grid-3d
  [grid units radius]
  (let [ball-maker (partial make-ball units radius)]
    (map-indexed ball-maker grid)))

(defn ball-box [units radius shrinkage]
  (let [nx 3 ny 3 nz 3
        dim (fn [n] (- (* (dec n) units) (* 2 shrinkage)))
        pos #(+ (/ units 2) shrinkage)
        radius (- radius shrinkage)]
    (union
      (->> (cube (dim nx) (dim ny) (dim nz) :center false)
           (translate [(pos) (pos) (pos)]))
      (ball-grid-3d (filter #(not= [1 1 1] %) (grid-3d nx ny nz)) units radius))))

(defn output []
  (let [units 17
        radius 8
        height (* 3 units)]
    (difference
      (ball-box units radius 0)
      (ball-box units radius 1)
      (->> (cylinder (/ radius 2) height :center false)
           (translate [units units 0])))))

(spit "output/pandora.scad" (write-scad (output)))
