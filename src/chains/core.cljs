(ns chains.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            
            [chains.hex :as hex]
            [chains.layout :as layout]
            ))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(def neighborhood
  [(hex/hex 0 0 0)
   (hex/hex 0 1 -1)
   (hex/hex 0 -1 1)
   (hex/hex 1 0 -1)
   (hex/hex 1 -1 0)
   (hex/hex -1 0 1)
   (hex/hex -1 1 0)])

;; to access default layout... layout/default-layout
(defn render-hex [l h]
  (let [verts (layout/corners l h)]
    (q/stroke 0 128 0)
    (q/stroke-weight 5)
    (q/fill 200 240 200)
    (q/begin-shape)
    (doseq [p verts]
      (q/vertex (p :x) (p :y)))
    (q/end-shape :close)))

(defn draw-state [state]
  (doseq [h neighborhood]
    (render-hex layout/default-layout h)))

; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch chains
    :host "chains"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    ;:update update-state
    :draw draw-state
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
