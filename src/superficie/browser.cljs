(ns superficie.browser
  "Browser entry point — exposes the superficie API on the global
   `superficie` object for use in <script> tags."
  (:require [superficie.api :as api]))

;; Expose on globalThis.superficie — the npm API, so options objects
;; ({width, context}) work the same in both
(set! (.-superficie js/globalThis)
      #js {:renderString  api/toSup
           :parseString   api/toClj
           :toSup         api/toSup
           :toClj         api/toClj
           :supToForms    api/supToForms
           :registerShape api/registerShape})
