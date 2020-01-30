(ns xpath-finder.content-script
  (:require-macros [chromex.support :refer [runonce]])
  (:require [xpath-finder.content-script.core :as core]))

(runonce
  (core/init!))
