(ns chains.layout
  (:require [chains.hex :as hex]))

;;;; LAYOUT AND ORIENTATION FUNCTIONS

(def sqrt3 (Math.sqrt 3.0))

;; could probably just define the layout maps
;; without using the 'orientation' fn, but whatever
;; TODO: statically-defined basic layouts...?

(defn orientation [f0 f1 f2 f3 i0 i1 i2 i3 start-angle]
  ;; TODO: pretty confident there's a better way to do this that I'm forgetting
  
  {:f0 f0 :f1 f1
   :f2 f2 :f3 f3

   :i0 i0 :i1 i1
   :i2 i2 :i3 i3

   :start-angle start-angle})

(def pointy-tops
  ;; hexes with pointy tops and bottoms - sides parallel to y axis
  (orientation
    sqrt3 (/ sqrt3 2.0)
    0.0   (/ 3.0 2.0)

    (/ sqrt3 3.0) (/ -1.0 3.0) ;;bug?
    0.0           (/ 2.0 3.0)

    0.5))

(def flat-tops
  ;; hexes with flat tops and bottoms - top/bottom parallel to x axis
  (orientation
    (/ 3.0 2.0)   0.0
    (/ sqrt3 2.0) sqrt3

    (/ 2.0 3.0)  0.0 
    (/ -1.0 3.0) (/ sqrt3 3.0)

    0.0))

(defn point [x y]
  ;; helper function to create points
  {:x x
   :y y})

(defn layout [orientation size origin]
  ;; defines the hex grid arrangement
  {:orientation orientation
   :size size
   :origin origin})

(def default-layout
  (layout 
    flat-tops
    (point 50 50) ;; x and y scaling, I think...
    (point 100 100) ;; origin
    )
  )

;;;; COORDINATE CHANGING FUNCTIONS

(defn cube-round [{:keys [q r s]}]
  ;; to ensure that the final hex is all integeriffic
  (let [rq (Math.round q)
        rr (Math.round r)
        rs (Math.round s)

        dq (Math.abs (- rq q))
        dr (Math.abs (- rr r))
        ds (Math.abs (- rs s))]
    
    (if (and (> dq dr) (> dq ds))
      (hex/hex (* -1 (+ rr rs)) rr rs)
      (if (> dr ds)
        (hex/hex rq (* -1 (+ rq rs)) rs)
        (hex/hex rq rr (* -1 (+ rq rr)))))))

(defn hex-to-pixel [{:keys [orientation size origin]} h]
  ;; given a hex, find a single pixel that represents it
  ;; i think this gives the top left "corner" of the hex?
  ;; TODO: figure that out
  (let [M orientation
        x-scale (size :x)
        y-scale (size :y)
        x (* x-scale (+ (* (M :f0) (h :q)) (* (M :f1) (h :r))))
        y (* y-scale (+ (* (M :f2) (h :q)) (* (M :f3) (h :r))))]
    
    (point (+ x (origin :x)) (+ y (origin :y)))))

(defn pixel-to-hex [{:keys [orientation size origin]} p]
  ;; the inverse of hex-to-pixel: given a pixel, find the corresponding hex
  ;; in the implementation guide (section 2.2) it's indicated that this will be
  ;; a "fractional hex" - gotta round this shit before turning it into a true hex
  (let [M orientation
        x-scale (size :x)
        y-scale (size :y)
        pt (point (/ (- (p :x) (origin :x)) x-scale)
                  (/ (- (p :y) (origin :y)) y-scale))
        q (+ (* (M :b0) (pt :x)) (* (M :b1) (pt :y)))
        r (+ (* (M :b2) (pt :x)) (* (M :b3) (pt :y)))]
    
    (cube-round (hex/hex q r))))

;; the following functions (corner-offset and corner) might go better in chains.hex...
;; it also seems like they can be combined and/or simplified in some way
;; TODO: make point adding function?

(defn corner-offset [{:keys [orientation size]} corner]
  ;; given an angle and an orientation (including a size!) find a corner's offset
  ;; the "corner" is a number 0-5 as can be seen in the next function
  (let [angle (* 2.0 Math.PI (+ corner (orientation :start-angle)) (/ 1.0 6.0))]
    (point (* (Math.cos angle) (size :x))
           (* (Math.sin angle) (size :y)))))

(defn corners [layout h]
  ;; given a particular hex and a layout configuration, get its corner points
  (let [center (hex-to-pixel layout h)
        offsets (map (partial corner-offset layout) (range 6))]
    (map 
      (fn [o]
        (point (+ (center :x) (o :x))
               (+ (center :y) (o :y))))
      offsets)))
