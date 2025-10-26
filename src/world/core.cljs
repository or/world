(ns world.core
  (:require
   [re-frame.core :as rf]
   [reagent.dom.client :as r]))

(rf/reg-event-db ::initialize
  (fn [db _]
    db))

(defonce ^:dynamic *app-root*
  nil)

(defn app-root []
  (when *app-root*
    (r/unmount *app-root*))
  (set! *app-root* (r/create-root (.getElementById js/document "app")))
  *app-root*)

(defn app []
  [:<>
   "hello!"])

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (r/render (app-root) [app]))
