(ns superficie.browser
  "Browser entry point — exposes the superficie API on the global
   `superficie` object for use in <script> tags."
  (:require [superficie.core :as core]))

;; Expose on globalThis.superficie
(set! (.-superficie js/globalThis)
      #js {:renderString  core/clj->sup
           :parseString   core/sup->clj
           :toSup         core/clj->sup
           :toClj         core/sup->clj})
