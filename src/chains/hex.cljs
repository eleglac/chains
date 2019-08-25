(ns chains.hex
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]

            ))

;; all code adapted from the hexagon grid guide at redblobgames.com

;; TODO: type hinting
;; TODO: docstrings!
;; TODO: wrap asserts in try/catches?

(defn hex
  ;; two inputs = axial coord system
  ([q r]
   (let [s (- (- 0 q) r)]
     (hex q r s)))
  
  ;; three inputs = cubic coord system
  ([q r s]
    ;; cube coordinate validity check 
    (assert (= 0 (+ q r s)) "invalid hex coordinates!")
  
    ;; a hex is a map defined by its coordinates
    {:q q 
     :r r 
     :s s}))

(def directions
  ;; map of offsets for the six neighboring hex directions
  {0 (hex 1 0 -1)
   1 (hex 1 -1 0)
   2 (hex 0 -1 1)
   3 (hex -1 0 1)
   4 (hex -1 1 0)
   5 (hex 0 1 -1)})

(defn direction [dir]
  ;; wrapper function around directions map to prevent nil results
  ;; if a bad key is provided
  ;; gotta assert that dir is an int, tho!
  (directions (mod dir 6)))

(defn add
  ;; add two hexes together
  ([hex-a hex-b] (apply merge-with + [hex-a hex-b]))
  
  ;; add multiple hexes together
  ([hexes] (apply merge-with + hexes)))

(defn subtract
  ;; subtract hex B from hex A
  ([hex-a hex-b] (apply merge-with - [hex-a hex-b]))

  ;; not sure if necessary, but it is possible to serially subtract hexes!
  ([hexes] (apply merge-with - hexes)))

(defn multiply [hex scale]
  ;; 'scale' should probably always be an int, but not checked currently
  (reduce 
    (fn [m [k v]] (assoc m k (* scale v)))
    {} hex))

(defn length [hex]
  ;; presumably this is a length from the origin hex
  (let [sum (reduce + (map Math.abs (vals hex)))]
    (Math.floor (/ sum 2))))

(defn distance [hex-a hex-b]
  ;; a distance function based on other hex functions!
  (length (subtract hex-a hex-b)))

(defn neighbor [hex dir]
  ;; find the neighbor of 'hex' in direction 'dir'
  ;; dir can be any integer
  (add hex (direction dir)))

