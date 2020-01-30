(ns xpath-finder.popup
  (:require-macros [chromex.support :refer [runonce]])
  (:require [xpath-finder.popup.core :as core]))

(runonce
  (core/init!))
