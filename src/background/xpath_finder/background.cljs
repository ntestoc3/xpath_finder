(ns xpath-finder.background
  (:require-macros [chromex.support :refer [runonce]])
  (:require [xpath-finder.background.core :as core]))

(runonce
  (core/init!))
