(ns xpath-finder.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [<!]]
            [xpath-finder.robula :refer [get-robust-xpath]]
            [clojure.string :as string]
            [dommy.core :as dommy :refer-macros [sel sel1]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                               oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols.chrome-port :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]))

; -- a message loop ---------------------------------------------------------------------------------------------------------

(defn process-message! [message]
  (log "CONTENT SCRIPT: got message:" message))

(defn run-message-loop! [message-channel]
  (log "CONTENT SCRIPT: starting message loop...")
  (go-loop []
    (when-some [message (<! message-channel)]
      (process-message! message)
      (recur))
    (log "CONTENT SCRIPT: leaving message loop")))

; -- a simple page analysis  ------------------------------------------------------------------------------------------------

(defn do-page-analysis! [background-port]
  (let [script-elements (.getElementsByTagName js/document "a")
        script-count (.-length script-elements)
        title (.-title js/document)
        msg (str "CONTENT SCRIPT: document '" title "' contains " script-count " a tags.")]
    (log msg)
    (post-message! background-port msg)))

(defn connect-to-background-page! []
  (let [background-port (runtime/connect)]
    (post-message! background-port "hello from CONTENT SCRIPT!")
    (run-message-loop! background-port)
    (do-page-analysis! background-port)))

; -- main entry point -------------------------------------------------------------------------------------------------------
(def last-ele (atom nil))

(defn remove-mark
  [ele]
  (when ele
    (dommy/remove-style! ele :box-shadow)
    (reset! last-ele nil)))

(defn mark-element
  [ele]
  (when ele
    (dommy/set-style! ele :box-shadow "0px 0px 2px 2px blue")
    (reset! last-ele ele)))

(defn handle-mouse-move
  [e]
  (let [x (.-clientX e)
        y (.-clientY e)]
    (log "x:" x " y:" y)
    (if-let [ele (js/document.elementFromPoint  x y)]
      (do
        (when-let [prev-ele @last-ele]
          (when-not (= ele prev-ele)
            (remove-mark prev-ele)))
        (mark-element ele)
        (log "elements xpath:" (get-robust-xpath js/document ele)))
      (remove-mark @last-ele))))

(defn hook-event
  ([event-name event-listener] (hook-event event-name event-listener true))
  ([event-name event-listener use-capture]
   (js/document.addEventListener (name event-name) event-listener use-capture)))

(defn init! []
  (log "CONTENT SCRIPT: init")
  (dommy/listen! js/document :DOMContentLoaded (fn []
                                                 (log "dom content loaded!")
                                                 (hook-event :mousemove handle-mouse-move)
                                                 (connect-to-background-page!)))
  )


(comment
  (hook-event :mousemove handle-mouse-move)


  )
