(ns sandbox.crime
  (:require [cheshire.core :as json]))

(defn load-json
  "Given a filename, reads a JSON file and returns it, parsed, with keywords."
  [file]
  (json/parse-string (slurp file) true))

(def fips
  "A map of FIPS codes to their county names."
  (->> "fips.json"
       load-json
       :table
       :rows
       (into {})))

(defn fips-code
  "Given a county (a map with :fips_state_code and :fips_county_code keys),
  returns the five-digit FIPS code for the county, as a string."
  [county]
  (str (:fips_state_code county) (:fips_county_code county)))

(defn most-duis
  "Given a JSON filename of UCR crime data for a particular year, finds the
  counties with the most DUIs."
  [file]
  (->> file
       load-json
       (sort-by :driving_under_influence)
       (take-last 10)
       (map (fn [county]
              [(fips (fips-code county))
               (:driving_under_influence county)]))
       (into {})))

; most-duis tells us about the raw number of reports, but doesn’t account for differences in county population. One would naturally expect counties with more people to have more crime! Divide the :driving_under_influence of each county by its :county_population to find a prevalence of DUIs, and take the top ten counties based on prevalence. How should you handle counties with a population of zero?

; How do the prevalence counties compare to the original counties? Expand most-duis to return vectors of [county-name, prevalence, report-count, population] What are the populations of the high-prevalence counties? Why do you suppose the data looks this way? If you were leading a public-health campaign to reduce drunk driving, would you target your intervention based on report count or prevalence? Why?

; We can generalize the most-duis function to handle any type of crime. Write a function most-prevalent which takes a file and a field name, like :arson, and finds the counties where that field is most often reported, per capita.

; Write a test to verify that most-prevalent is correct.
