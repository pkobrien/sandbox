(ns app.core
  (:require
    [clojure.string :as string]
    [goog.dom :as dom]
    [goog.object]
    [hipo.core :as hipo]
    [ion.poly.core :as poly]
    [monet.canvas :as canvas]
    [shodan.console :as console :include-macros true]
    [shodan.inspection :refer [inspect]]))

(enable-console-print!)


;; ---------------------------------------------------------------------
;; Basic functions
;;

(defn degrees
  "Returns the angle rad in degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn radians
  "Returns the angle deg in radians."
  [deg]
  (* (/ deg 180) Math/PI))


;; ---------------------------------------------------------------------
;; Parametric equations
;;

(defn circle-x
  "Returns the x location at angle t (in radians) on the circumference
  of the circle with radius r."
  [r t]
  (* r (Math/cos t)))

(defn circle-y
  "Returns the y location at angle t (in radians) on the circumference
  of the circle with radius r."
  [r t]
  (* r (Math/sin t)))

(defn epitrochoid-x
  "Returns the x location at angle t (in radians) of the epitrochoid
  with fixed circle radius R (in radians), rolling circle radius r (in
  radians), and length of radial d."
  [R r d t]
  (- (* (+ R r) (Math/cos t))
     (* d (Math/cos (* (/ (+ R r)
                          r)
                       t)))))

(defn epitrochoid-y
  "Returns the y location at angle t (in radians) of the epitrochoid
  with fixed circle radius R (in radians), rolling circle radius r (in
  radians), and length of radial d."
  [R r d t]
  (- (* (+ R r) (Math/sin t))
     (* d (Math/sin (* (/ (+ R r)
                          r)
                       t)))))


;; -----------------------------------------------------------------------------
;; Elements

(defn app-div [] (poly/get-element :app))

(defn slider [state param value min max]
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :on-input (fn [e]
                       (swap! state assoc param (.-target.value e)))}])


;; -----------------------------------------------------------------------------
;; Bounce Animation

(defn ball-position [{:keys [delta-t-ms speed-pps x y w h direction]}]
  (let [direction (radians direction)
        pixels-per-ms (/ speed-pps 1000)
        delta-pixels (* delta-t-ms pixels-per-ms)
        dx (circle-x delta-pixels direction)
        dy (circle-y delta-pixels direction)
        clamped-x (-> x (+ dx) (max 0) (min w))
        clamped-y (-> y (+ dy) (max 0) (min h))]
    {:x clamped-x :y clamped-y}))

(defn ball-direction [{:keys [w h x y direction]}]
  (cond
    (not (< 0 x w)) (- 180 direction)
    (not (< 0 y h)) (- 360 direction)
    :else direction))

(defn bounce-update! [state timestamp]
  (if (:timestamp @state)
    (swap! state assoc :delta-t-ms (- timestamp (:timestamp @state))))
  (swap! state assoc :timestamp timestamp)
  (swap! state merge (ball-position @state))
  (swap! state assoc :direction (ball-direction @state)))

(defn draw-background
  [{:keys [ctx w h persist-image?]}]
  (when-not persist-image?
    (-> ctx
        (canvas/fill-style "rgba(25,29,33,0.75)")
        (canvas/fill-rect {:x 0 :y 0 :w w :h h}))))

(defn draw-ball
  [{:keys [ctx ball-color ball-size x y]}]
  (canvas/fill-style ctx ball-color)
  (canvas/circle ctx {:x x :y y :r ball-size})
  (canvas/fill-rect ctx {:x x :y y :w ball-size :h ball-size}))

(defn bounce-render! [data]
  (let [display-items [(:update? data) (:timestamp data)]]
    (set! (.-innerText (app-div)) (string/join ", " display-items)))
  (draw-background data)
  (draw-ball data))

(defn init-bounce-state []
  {:ball-color "red"
   :ball-size 10
   :delta-t-ms 0
   :direction (rand-int 360)
   :persist-image? false
   :speed-pps 50
   :timestamp nil
   :w 320 :h 320
   :x 10 :y 10})

(defonce bounce-state
  (atom (merge {:canvas nil
                :ctx nil
                :update? true
                :render? true} (init-bounce-state))))

(defn reset-bounce-state []
  (swap! bounce-state merge (init-bounce-state)))


;; -----------------------------------------------------------------------------
;; App State

(defonce app-state
  (atom
    {:name "Animating"
     :animation-frame-loop-alive? (atom false)
     :animation-frame-loop-active? (atom true)
     }))


;; -----------------------------------------------------------------------------
;; Animation Cycle

(defn animate! [timestamp]
  (when (:update? @bounce-state) (bounce-update! bounce-state timestamp))
  (when (:render? @bounce-state) (bounce-render! @bounce-state)))

(defn start-looping! []
  (let [activate? (:animation-frame-loop-active? @app-state)]
    (let [[alive? active?] (poly/animation-frame-loop! animate! @activate?)]
      (swap! app-state assoc
             :animation-frame-loop-alive? alive?
             :animation-frame-loop-active? active?))))

(defn stop-looping! []
  (reset! (:animation-frame-loop-alive? @app-state) false))

(defn start-animating! []
  (reset! (:animation-frame-loop-active? @app-state) true))

(defn stop-animating! []
  (reset! (:animation-frame-loop-active? @app-state) false))

(defn toggle-animating! []
  (swap! (:animation-frame-loop-active? @app-state) not))


;; -----------------------------------------------------------------------------
;; Init / Load / Setup / Teardown

(defn setup []
  (console/info "setup")
  (poly/set-title! (:app-name @app-state))
  (start-looping!))

(defn teardown []
  (console/info "teardown")
  (stop-looping!))

(defn ^:export on-load []
  (console/info "on-load")
  (teardown)
  (setup))

(defn ^:export on-init []
  (console/info "on-init")
  (let [div (hipo/create
              [:div
               [:h3 "Bouncing"]
               [:div
                [:button {:on-click toggle-animating!} "Start/Stop"]
                [:button {:on-click reset-bounce-state} "Reset"]]
               [:div
                [:span (str "Ball Size: ")]
                (slider bounce-state :ball-size 5 5 100)]
               [:div
                [:span (str "Ball Speed: ")]
                (slider bounce-state :speed-pps 30 1 200)]
               [:div
                [:canvas#bounce-canvas {:width (:w @bounce-state) :height (:h @bounce-state)}]]])]
    (dom/appendChild (poly/get-body) div))
  (let [canvas (poly/get-element "bounce-canvas")
        context (.getContext canvas "2d")]
    (swap! bounce-state assoc :canvas canvas)
    (swap! bounce-state assoc :ctx context))
  (setup)
  (inspect @app-state)
  (inspect @bounce-state))
