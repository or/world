(ns world.core
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.core :as ra]
   [reagent.dom.client :as r]))

(defonce zoom (ra/atom 1.0))
(defonce translate (ra/atom [0 0]))
(defonce drag (ra/atom {:active? false
                        :start [0 0]
                        :base [0 0]}))
(def min-zoom 1)
(def max-zoom 20.0)

(defn clamp [v a b] (-> v (max a) (min b)))

(defn transform-string []
  (let [[tx ty] @translate s @zoom]
    (str "translate(" tx " " ty ") scale(" s ")")))

(defn svg-coords [svg e]
  (let [pt (.createSVGPoint svg)]
    (set! (.-x pt) (.-clientX e))
    (set! (.-y pt) (.-clientY e))
    (let [ctm (.getScreenCTM svg)
          inv (.inverse ctm)
          local (.matrixTransform pt inv)]
      [(.-x local) (.-y local)])))

(defn handle-wheel [svg e]
  (.preventDefault e)
  (.stopPropagation e)
  (let [[cx cy] (svg-coords svg e)
        old @zoom
        step 1.07
        factor (if (pos? (.-deltaY e))
                 (/ 1 step)
                 step)
        new (clamp (* old factor) min-zoom max-zoom)
        k (/ new old)
        [tx ty] @translate]
    (reset! translate
            [(+ (- (* k (- tx cx)) (- cx)))
             (+ (- (* k (- ty cy)) (- cy)))])
    (reset! zoom new)))

(defn on-mouse-down [e]
  (.preventDefault e)
  (.stopPropagation e)
  (reset! drag {:active? true
                :start [(.-clientX e) (.-clientY e)]
                :base @translate}))

(defn on-mouse-move [e]
  (when (:active? @drag)
    (.preventDefault e)
    (.stopPropagation e)
    (let [{:keys [start base]} @drag
          [sx sy] start
          [bx by] base
          dx (- (.-clientX e) sx)
          dy (- (.-clientY e) sy)]
      (reset! translate [(+ bx dx) (+ by dy)]))))

(defn on-mouse-up [e]
  (.preventDefault e)
  (.stopPropagation e)
  (swap! drag assoc :active? false))

(defonce water-color "#1f77b4")

(def country-colors
  ["#9dc3c2" "#a7c8a0" "#c5ca91" "#e0cfa3"
   "#e1b6a0" "#d4a3a3" "#c4a3b5" "#b4a3c6"
   "#a3aad0" "#a3bfd8" "#92bccc" "#8fbfb8"
   "#a1c1a9" "#c2c2a3" "#d3b7a3" "#c6aba3"])

(rf/reg-event-db
  ::set-countries
  (fn [db [_ countries]]
    (let [colored (mapv #(assoc % :fill (rand-nth country-colors)) countries)]
      (assoc db
             :loading? false
             :countries colored))))

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

(rf/reg-sub ::countries
  (fn [db _] (:countries db)))

(rf/reg-sub ::loading?
  (fn [db _] (:loading? db)))

(defn mercator-projection [lon lat width height]
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

(defn polygon->path [proj-fn coords]
  (when (seq coords)
    (str "M "
         (->> coords
              (map (fn [[lon lat]]
                     (let [[x y] (proj-fn lon lat)]
                       (str x "," y))))
              (str/join " L "))
         " Z")))

(defn country->paths [proj-fn country]
  (map #(polygon->path proj-fn %) (:polygons country)))

(rf/reg-sub
  ::projected-paths
  :<- [::countries]
  (fn [countries [_ idx projection]]
    (let [country (get countries idx)
          proj-fn (case projection
                    :mercator (fn [lon lat]
                                (mercator-projection lon lat 1000 1000))
                    (fn [lon lat]
                      (mercator-projection lon lat 1000 1000)))]
      (when country
        (country->paths proj-fn country)))))

(defn country [{:keys [idx projection]}]
  (let [paths @(rf/subscribe [::projected-paths idx projection])
        country (get @(rf/subscribe [::countries]) idx)]
    [:<>
     (for [p paths]
       ^{:key (hash p)}
       [:path {:d p
               :stroke "#333"
               :strokeWidth 0.5
               :fill (:fill country)
               :vectorEffect "non-scaling-stroke"}])]))

(defn world []
  (let [svg-ref (ra/atom nil)]
    (ra/create-class
     {:component-did-mount
      (fn [_]
        (when-let [svg @svg-ref]
          (.addEventListener svg "wheel"
                             (fn [e] (handle-wheel svg e))
                             #js {:passive false})))
      :reagent-render
      (fn []
        (let [countries @(rf/subscribe [::countries])]
          [:svg {:ref #(reset! svg-ref %)
                 :viewBox "0 0 1000 1000"
                 :style {:width "100vw"
                         :height "100vh"
                         :cursor (if (:active? @drag)
                                   "grabbing"
                                   "grab")}
                 :on-context-menu #(.preventDefault %)
                 :on-mouse-down on-mouse-down
                 :on-mouse-move on-mouse-move
                 :on-mouse-up on-mouse-up
                 :on-mouse-leave on-mouse-up}
           [:g {:transform (transform-string)}
            [:rect {:x 0
                    :y 0
                    :width 1000
                    :height 1000
                    :fill water-color}]
            (for [i (range (count countries))]
              ^{:key i} [country {:idx i
                                  :projection :mercator}])]]))})))

(defonce ^:dynamic *app-root* nil)

(defn app-root []
  (when *app-root* (r/unmount *app-root*))
  (set! *app-root* (r/create-root (.getElementById js/document "app")))
  *app-root*)

(defn app []
  (let [loading? @(rf/subscribe [::loading?])]
    (if loading? [:p "Loading..."] [world])))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (r/render (app-root) [app]))
