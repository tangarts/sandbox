(ns sandbox.rocket
  (:require [clojure.pprint :refer [pprint]]))

;; Linear algebra for {:x 1 :y 2 :z 3} coordinate vectors.

(defn map-values
  "Applies f to every value in the map m."
  [f m]
  (into {}
        (map (fn [pair]
               [(key pair) (f (val pair))])
             m)))

(defn magnitude
  "What's the radius of a given set of cartesian coordinates?"
  [c]
  ; By the Pythagorean theorem...
  (Math/sqrt (+ (Math/pow (:x c) 2)
                (Math/pow (:y c) 2)
                (Math/pow (:z c) 2))))

(defn scale
  "Multiplies a map of x, y, and z coordinates by the given factor."
  [factor coordinates]
  (map-values (partial * factor) coordinates))

(defn unit-vector
  "Scales coordinates to magnitude 1."
  [coordinates]
  (scale (/ (magnitude coordinates)) coordinates))

(defn dot-product
  "Finds the inner product of two x, y, z coordinate maps. See
  http://en.wikipedia.org/wiki/Dot_product"
  [c1 c2]
  (+ (* (:x c1) (:x c2))
     (* (:y c1) (:y c2))
     (* (:z c1) (:z c2))))

(defn projection
  "The component of coordinate map a in the direction of coordinate map b.
  See http://en.wikipedia.org/wiki/Vector_projection."
  [a b]
  (let [b (unit-vector b)]
    (scale (dot-product a b) b)))

(defn rejection
  "The component of coordinate map a *not* in the direction of coordinate map
  b."
  [a b]
  (let [a' (projection a b)]
    {:x (- (:x a) (:x a'))
     :y (- (:y a) (:y a'))
     :z (- (:z a) (:z a'))}))

;; Coordinate conversion

(defn cartesian->spherical
  "Converts a map of Cartesian coordinates :x, :y, and :z to spherical
  coordinates :r, :theta, and :phi."
  [c]
  (let [r (magnitude c)]
    {:r     r
     :phi   (Math/acos (/ (:z c) r))
     :theta (Math/atan (/ (:y c) (:x c)))}))

(defn spherical->cartesian
  "Converts spherical to Cartesian coordinates."
  [c]
  {:x (* (:r c) (Math/cos (:theta c)) (Math/sin (:phi c)))
   :y (* (:r c) (Math/sin (:theta c)) (Math/sin (:phi c)))
   :z (* (:r c) (Math/cos (:phi c)))})

;; The earth

(def earth-equatorial-radius
  "Radius of the earth, in meters"
  6378137)

(def earth-day
  "Length of an earth day, in seconds."
  86400)

(def earth-equatorial-speed
  "How fast points on the equator move, relative to the center of the earth, in
  meters/sec."
  (/ (* 2 Math/PI earth-equatorial-radius)
     earth-day))

(def earth-mass
  "Mass of earth in kg"
  5.9722E24)

(def G "Gravitational constant" 6.674E-11)

(def g "Acceleration of gravity in meters/s^2" -9.8)

(def initial-space-center
  "The initial position and velocity of the launch facility"
  {:time     0
   :position {:x earth-equatorial-radius
              :y 0
              :z 0}
   :velocity {:x 0
              :y earth-equatorial-speed
              :z 0}})

;; Craft

(defn centaur
  "The upper rocket stage.
  http://en.wikipedia.org/wiki/Centaur_(rocket_stage)
  http://www.astronautix.com/stages/cenaurde.htm"
  []
  {:dry-mass  2361
   :fuel-mass 13897
   :isp       4354
   :max-fuel-rate (/ 13897 470)})

(defn atlas-v
  "The full launch vehicle. http://en.wikipedia.org/wiki/Atlas_V"
  [next-stage]
  {:dry-mass  50050
   :fuel-mass 284450
   :isp 3050
   :max-fuel-rate (/ 284450 253)
   :next-stage next-stage})

;; Flight control

(def ascent
  "The start and end times for the ascent burn."
  [0 300])

(def circularization
  "The start and end times for the circularization burn."
  [400 1000])

(defn orientation
  "What direction is the craft pointing?"
  [craft]
  (cond
    ; Initially, point along the *position* vector of the craft--that is
    ; to say, straight up, away from the earth.
    (<= (first ascent) (:time craft) (last ascent))
    (:position craft)

    ; During the circularization burn, we want to burn *sideways*, in the
    ; direction of the orbit. We'll find the component of our velocity
    ; which is aligned with our position vector (that is to say, the vertical
    ; velocity), and subtract the vertical component. All that's left is the
    ; *horizontal* part of our velocity.
    (<= (first circularization) (:time craft) (last circularization))
    (rejection (:velocity craft) (:position craft))

    ; Otherwise, just point straight ahead.
    :else (:velocity craft)))

(defn fuel-rate
  "How fast is fuel, in kilograms/second, consumed by the craft?"
  [craft]
  (cond
    ; Out of fuel
    (<= (:fuel-mass craft) 0)
    0

    ; Ascent burn
    (<= (first ascent) (:time craft) (last ascent))
    (:max-fuel-rate craft)

    ; Circularization burn
    (<= (first circularization) (:time craft) (last circularization))
    (:max-fuel-rate craft)

    ; Shut down engines otherwise
    :else 0))

(defn stage
  "When fuel reserves are exhausted, separate stages. Otherwise, return craft
  unchanged."
  [craft]
  (cond
    ; Still fuel left
    (pos? (:fuel-mass craft))
    craft

    ; No remaining stages
    (nil? (:next-stage craft))
    craft

    ; Stage!
    :else
    (merge (:next-stage craft)
           (select-keys craft [:time :position :velocity]))))

;; Dynamics

(defn thrust
  "How much force, in newtons, does the craft's rocket engines exert?"
  [craft]
  (* (fuel-rate craft) (:isp craft)))

(defn mass
  "The total mass of a craft."
  [craft]
  (+ (:dry-mass craft) (:fuel-mass craft)))

(defn gravity-force
  "The force vector, each component in Newtons, due to gravity."
  [craft]
  ; Since force is mass times acceleration...
  (let [total-force (* g (mass craft))]
    (-> craft
        ; Now we'll take the craft's position
        :position
        ; in spherical coordinates,
        cartesian->spherical
        ; replace the radius with the gravitational force...
        (assoc :r total-force)
        ; and transform back to Cartesian-land
        spherical->cartesian)))

(declare altitude)

(defn engine-force
  "The force vector, each component in Newtons, due to the rocket engine."
  [craft]
;  Debugging; useful for working through trajectories in detail.
;  (println craft)
;  (println "t   " (:time craft) "alt" (altitude craft) "thrust" (thrust craft))
;  (println "fuel" (:fuel-mass craft))
;  (println "vel " (:velocity craft))
;  (println "ori " (unit-vector (orientation craft)))
  (scale (thrust craft) (unit-vector (orientation craft))))

(defn total-force
  "Total force on a craft."
  [craft]
  (merge-with + (engine-force craft)
              (gravity-force craft)))

(defn acceleration
  "Total acceleration of a craft."
  [craft]
  (let [m (mass craft)]
    (scale (/ m) (total-force craft))))

(defn step
  [craft dt]
  (let [craft (stage craft)]
    (assoc craft
           ; Time advances by dt seconds
           :time      (+ dt (:time craft))
           ; We burn some fuel
           :fuel-mass (- (:fuel-mass craft) (* dt (fuel-rate craft)))
           ; Our position changes based on our velocity
           :position  (merge-with + (:position craft)
                                  (scale dt (:velocity craft)))
           ; And our velocity changes based on our acceleration
           :velocity  (merge-with + (:velocity craft)
                                  (scale dt (acceleration craft))))))

;; Launch and flight

(defn prepare
  "Prepares a craft for launch from an equatorial space center."
  [craft]
  (merge craft initial-space-center))

(defn trajectory
  [dt craft]
  "Returns all future states of the craft, at dt-second intervals."
  (iterate #(step % dt) craft))

;; Analyzing trajectories

(defn altitude
  "The height above the surface of the equator, in meters."
  [craft]
  (-> craft
      :position
      cartesian->spherical
      :r
      (- earth-equatorial-radius)))

(defn above-ground?
  "Is the craft at or above the surface?"
  [craft]
  (<= 0 (altitude craft)))

(defn flight
  "The above-ground portion of a trajectory."
  [trajectory]
  (take-while above-ground? trajectory))

(defn crashed?
  "Does this trajectory crash into the surface before 10 hours are up?"
  [trajectory]
  (let [time-limit (* 10 3600)] ; 10 hours
    (not (every? above-ground?
                 (take-while #(<= (:time %) time-limit) trajectory)))))

(defn crash-time
  "Given a trajectory, returns the time the rocket impacted the ground."
  [trajectory]
  (:time (last (flight trajectory))))

(defn apoapsis
  "The highest altitude achieved during a trajectory."
  [trajectory]
  (->> (map altitude (flight trajectory))
       (partition 2 1)
       (drop-while #(neg? (- (first %) (second %))))
       (ffirst)))

(defn periapsis
  "The lowest altitude achieved during a trajectory."
  [trajectory]
  (->> (map altitude (flight trajectory))
       (partition 2 1)
       (drop-while #(pos? (- (first %) (second %))))
       (ffirst)))

(defn apoapsis-time
  "The time of apoapsis"
  [trajectory]
  (:time (apply max-key altitude (flight trajectory))))

; scratch ------------------------------------------------------------

(-> (atlas-v (centaur)) prepare pprint)


