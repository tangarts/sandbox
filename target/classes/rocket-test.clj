(ns sandbox.rocket-test
  #_{:clj-kondo/ignore [:refer-all]}
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer [deftest is testing]]
            [sandbox.rocket :refer :all]))

(deftest spherical-coordinate-test
  (testing "spherical->cartesian"
    (is (= (spherical->cartesian {:r 2
                                  :phi 0
                                  :theta 0})
           {:x 0.0 :y 0.0 :z 2.0})))

  (testing "roundtrip"
    (let [pos {:x 1.0 :y 2.0 :z 3.0}]
      (is (= pos (-> pos cartesian->spherical spherical->cartesian))))))

(deftest makes-orbit
  (let [trajectory (->> (atlas-v (centaur))
                        prepare
                        (trajectory 1))]

    (when (crashed? trajectory)
      (println "Crashed at" (crash-time trajectory) "seconds")
      (println "Maximum altitude" (apoapsis trajectory)
               "meters at"        (apoapsis-time trajectory) "seconds"))

    ; Assert that the rocket eventually made it to orbit.
    (is (not (crashed? trajectory)))))

(deftest inflections
  (let [trajectory (->> (atlas-v (centaur))
                        prepare
                        (trajectory 1))]
    (testing "apoapsis"
      (is (= (apoapsis trajectory) 893100.773158608)))
    (testing "periapsis"
      (is (= (periapsis trajectory) 0.0)))))

; scratch ---------------------------------------------
