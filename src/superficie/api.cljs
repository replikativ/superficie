(ns superficie.api
  "Public JS API for the superficie npm package."
  (:require [cljs.reader :as reader]
            [superficie.core :as core]
            [superficie.shapes :as shapes]))

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

(defn ^:export registerShape
  "Register how a library macro renders as a block, as superficie's JVM
   shapes.edn does. qsym is the macro's qualified name, shape and options
   are EDN strings, e.g.
     registerShape(\"my.lib/defkernel\", \"[:name :params :body]\", \"{:index aget}\")
   Throws on an invalid shape or options."
  ([qsym shape] (registerShape qsym shape nil))
  ([qsym shape options]
   (shapes/register-shape! (symbol qsym)
                           (reader/read-string shape)
                           (when options (reader/read-string options)))
   nil))

;; Legacy exports for backward compatibility
(def ^:export renderString toSup)
(def ^:export parseString  toClj)
