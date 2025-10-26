(ns world.core
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.dom.client :as r]))

(defonce water-color
  "#1f77b4")

(def country-colors
  ["#9dc3c2" ; teal pastel
   "#a7c8a0" ; soft green
   "#c5ca91" ; muted chartreuse
   "#e0cfa3" ; sand beige
   "#e1b6a0" ; warm peach
   "#d4a3a3" ; dusty rose
   "#c4a3b5" ; mauve
   "#b4a3c6" ; lilac
   "#a3aad0" ; periwinkle
   "#a3bfd8" ; calm sky blue
   "#92bccc" ; ocean blue‑gray
   "#8fbfb8" ; desaturated turquoise
   "#a1c1a9" ; pistachio
   "#c2c2a3" ; soft khaki
   "#d3b7a3" ; rosy beige
   "#c6aba3"]) ; clay neutral

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

(defn mercator-projection
  "Project [lon lat] (in degrees) into [x y] for an SVG width×height box."
  [lon lat width height]
  (let [lambda (* lon (/ Math/PI 180))
        clamped-lat (max (min lat 85.0) -85.0)
        phi-clamped (* clamped-lat (/ Math/PI 180))
        x (* (/ (+ lambda Math/PI) (* 2 Math/PI)) width)
        y-scale (/ height (* 2 Math/PI))
        y (- (/ height 2)
             (* y-scale
                (Math/log
                 (Math/tan
                  (+ (/ Math/PI 4)
                     (/ phi-clamped 2))))))]
    [x y]))

(defn polygon->path
  "Convert a list of [lon lat] pairs into an SVG path string using proj-fn."
  [proj-fn coords]
  (when (seq coords)
    (str "M "
         (->> coords
              (map (fn [[lon lat]]
                     (let [[x y] (proj-fn lon lat)]
                       (str x "," y))))
              (str/join " L "))
         " Z")))

(defn country->paths
  "Convert each polygon in a country into an SVG path string."
  [proj-fn country]
  (map #(polygon->path proj-fn %) (:polygons country)))

(defn world []
  (let [countries @(rf/subscribe [::countries])
        proj (fn [lon lat] (mercator-projection lon lat 1000 1000))]
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
             :style {:fill water-color}}]

     [:g
      (doall
       (map-indexed
        (fn [i country]
          (let [fill-color (nth country-colors (mod i (count country-colors)))]
            (for [path (country->paths proj country)]
              ^{:key (str (:name country) "-" (hash path))}
              [:path {:d path
                      :stroke "#333"
                      :strokeWidth 0.5
                      :fill fill-color
                      :vectorEffect "non-scaling-stroke"}])))
        countries))]]))

(defn app []
  (let [loading? @(rf/subscribe [::loading?])]
    (if loading?
      [:p "Loading..."]
      [world])))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (r/render (app-root) [app]))
