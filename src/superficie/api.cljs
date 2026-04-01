(ns superficie.api
  "Public JS API for the superficie npm package."
  (:require [superficie.core :as core]))

(defn ^:export toSup
  "Convert a Clojure source string to superficie syntax. Returns a string."
  [source]
  (core/clj->sup source))

(defn ^:export toClj
  "Convert a superficie source string to Clojure source. Returns a string."
  [source]
  (core/sup->clj source))

(defn ^:export supToForms
  "Parse a superficie source string, return forms as a JS array of EDN strings."
  [source]
  (clj->js (mapv pr-str (core/sup->forms source))))

;; Legacy exports for backward compatibility
(def ^:export renderString toSup)
(def ^:export parseString  toClj)
