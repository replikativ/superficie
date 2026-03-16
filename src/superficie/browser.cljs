(ns superficie.browser
  "Browser entry point — exposes renderString and parseString on
   the global `superficie` object for use in <script> tags."
  (:require [clojure.string :as str]
            [superficie.render :as render]
            [superficie.parse :as parse]))

(defn render-string [source]
  (render/render-string source))

(defn parse-string [source]
  (parse/emit-source (parse/parse-string source)))

;; Expose on globalThis.superficie
(set! (.-superficie js/globalThis)
      #js {:renderString render-string
           :parseString  parse-string})
