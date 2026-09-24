(ns superficie.api
  "Public JS API for the superficie npm package."
  (:require [superficie.core :as core]))

(defn ^:export toSup
  "Convert a Clojure source string to superficie syntax. Returns a string.
   Optional opts object: {width: 80, context: \"(require '[raster.core :refer [deftm]])\"}
   — context holds the ns/require forms a snippet assumes but does not contain."
  ([source] (core/clj->sup source))
  ([source opts]
   (core/clj->sup source (js->clj opts :keywordize-keys true))))

(defn ^:export toClj
  "Convert a superficie source string to Clojure source. Returns a string.
   Optional opts object: {context: \"(require '[raster.core :refer [deftm]])\"}."
  ([source] (core/sup->clj source))
  ([source opts]
   (core/sup->clj source (js->clj opts :keywordize-keys true))))

(defn ^:export supToForms
  "Parse a superficie source string, return forms as a JS array of EDN strings."
  [source]
  (clj->js (mapv pr-str (core/sup->forms source))))

;; Legacy exports for backward compatibility
(def ^:export renderString toSup)
(def ^:export parseString  toClj)
