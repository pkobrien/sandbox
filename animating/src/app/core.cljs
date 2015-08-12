(ns app.core
  (:require
    [clojure.string :as string]
    [goog.dom :as dom]
    [goog.object]
    [hipo.core :as hipo]
    [ion.poly.core :as poly]
    [shodan.console :as console :include-macros true]
    [shodan.inspection :refer [inspect]]))

(enable-console-print!)


;; -----------------------------------------------------------------------------
;; Animation Cycle

(defn animate! [step state timestamp]
  (if (:animate? @state)
    (step state timestamp)
    (console/info "animate! has stopped"))
  (:animate? @state))

(defn- animate* [state]
  (console/info "setting :animate? to true")
  (swap! state assoc :animate? true)
  (let [step (:animate! @state)
        anim (partial animate! step state)]
    (console/info "animate! has started")
    (poly/listen-animation-frame! anim)))

(defn start-animating! [state]
  (poly/request-animation-frame! #(animate* state)))

(defn stop-animating! [state]
  (console/info "setting :animate? to false")
  (swap! state assoc :animate? false))

(defn toggle-animating! [state]
  (if (:animate? @state)
    (stop-animating! state)
    (start-animating! state)))


;; -----------------------------------------------------------------------------
;; Elements

(defn app-div [] (poly/get-element :app))

(defn slider [state param value min max]
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :on-change (fn [e]
                        (swap! state assoc param (.-target.value e)))}])


;; -----------------------------------------------------------------------------
;; Bounce Animation

(defn bounce-update! [state timestamp]
  (swap! state assoc :timestamp timestamp))

(defn bounce-render! [state]
  (let [display [(:animate? @state) (:timestamp @state)]]
    (set! (.-innerText (app-div)) (string/join ", " display))))

(defn bounce-animate! [state timestamp]
  (bounce-update! state timestamp)
  (bounce-render! state))

(defonce bounce-state
  (atom
    {:animate! bounce-animate!
     :animate? false
     :canvas nil
     :context nil
     :timestamp nil
     :w 320 :h 320
     :x 1 :y 1
     }))


;; -----------------------------------------------------------------------------
;; App State

(defonce app-state
  (atom
    {:name "Animating"
     }))


;; -----------------------------------------------------------------------------
;; Init / Load / Setup / Teardown

(defn setup []
  (console/info "setup")
  (poly/set-title! (:app-name @app-state))
  (start-animating! bounce-state))

(defn teardown []
  (console/info "teardown")
  (stop-animating! bounce-state))

(defn ^:export on-load []
  (console/info "on-load")
  (teardown)
  (setup))

(defn ^:export on-init []
  (console/info "on-init")
  (let [div (hipo/create
              [:div
               [:button {:on-click (partial toggle-animating! bounce-state)} "Start/Stop"]
               [:canvas#bounce-canvas {:width (:w @bounce-state) :height (:h @bounce-state)}]])]
    (dom/appendChild (poly/get-body) div))
  (let [canvas (poly/get-element "bounce-canvas")
        context (.getContext canvas "2d")]
    (swap! bounce-state assoc :canvas canvas)
    (swap! bounce-state assoc :context context))
  (setup)
  (inspect @app-state)
  (inspect @bounce-state))
