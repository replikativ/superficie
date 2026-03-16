(ns superficie.api
  "Public JS API for the superficie npm package.
   Exposes renderString and parseString as plain JS functions."
  (:require [clojure.string :as str]
            [superficie.render :as render]
            [superficie.parse :as parse]))

(defn ^:export renderString
  "Convert a Clojure source string to superficie syntax.
   Returns a string."
  [source]
  (render/render-string source))

(defn ^:export parseString
  "Convert a superficie source string back to Clojure S-expressions.
   Returns a string of Clojure code."
  [source]
  (parse/emit-source (parse/parse-string source)))
