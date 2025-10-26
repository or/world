(ns world.core
  (:require
   [re-frame.core :as rf]
   [reagent.dom.client :as r]))

(defonce water-color
  "#1f77b4")

(defonce colors
  ["#ff7f0e",
   "#2ca02c",
   "#d62728",
   "#9467bd",
   "#8c564b",
   "#e377c2",
   "#7f7f7f",
   "#bcbd22"])

(rf/reg-event-db
  ::set-countries
  (fn [db [_ countries]]
    (assoc db
           :loading? false
           :countries countries)))

(rf/reg-event-fx
  ::initialize
  (fn [{:keys [db]} _]
    (if (:countries db)
      {:db db}
      {:db (assoc db :loading? true)
       :dispatch [::load-countries]})))

(rf/reg-event-fx
  ::load-countries
  (fn [_ _]
    {:fetch-countries "/countries.json"}))

(rf/reg-fx
  :fetch-countries
  (fn [url]
    (-> (js/fetch url)
        (.then (fn [resp]
                 (if (.-ok resp)
                   (.json resp)
                   (throw (js/Error.
                           (str "Failed to load " url
                                " (" (.-status resp) ")"))))))
        (.then (fn [data]
                 (rf/dispatch
                  [::set-countries
                   (js->clj data :keywordize-keys true)])))
        (.catch (fn [err]
                  (js/console.error "Failed to fetch countries:" err))))))

(rf/reg-sub
  ::countries
  (fn [db _]
    (:countries db)))

(rf/reg-sub
  ::loading?
  (fn [db _]
    (:loading? db)))

(defonce ^:dynamic *app-root* nil)

(defn app-root []
  (when *app-root*
    (r/unmount *app-root*))
  (set! *app-root* (r/create-root (.getElementById js/document "app")))
  *app-root*)

(defn world []
  (let [countries @(rf/subscribe [::countries])]
    [:svg {:version "1.1"
           :xmlns "http://www.w3.org/2000/svg"
           :xmlnsXlink "http://www.w3.org/1999/xlink"
           :viewBox "0 0 1000 1000"
           :preserveAspectRatio "xMidYMin slice"
           :style {:width "100vw"
                   :height "100vh"}}
     [:rect {:x1 0
             :y1 0
             :width 1000
             :height 1000
             :style {:fill water-color}}]]))

(defn app []
  (let [loading? @(rf/subscribe [::loading?])]
    (if loading?
      [:p "Loading..."]
      [world])))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (r/render (app-root) [app]))
