(ns world.core
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.core :as ra]
   [reagent.dom.client :as r]))

(defonce zoom
  (ra/atom 1.0))

(defonce translate
  (ra/atom [0 0]))

(defonce drag
  (ra/atom {:active? false
            :start [0 0]
            :base [0 0]}))

(def min-zoom
  1)

(def max-zoom
  20.0)

(defn clamp [v a b]
  (-> v (max a) (min b)))

(defn transform-string []
  (let [[tx ty] @translate
        s @zoom]
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
        new (clamp (* old factor)
                   min-zoom
                   max-zoom)
        k (/ new old)
        [tx ty] @translate]
    (reset! translate
            [(+ (- (* k (- tx cx))
                   (- cx)))
             (+ (- (* k (- ty cy))
                   (- cy)))])
    (reset! zoom new)))

(defn on-mouse-down [e]
  (.preventDefault e)
  (.stopPropagation e)
  (reset! drag
          {:active? true
           :start [(.-clientX e)
                   (.-clientY e)]
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
      (reset! translate
              [(+ bx dx)
               (+ by dy)]))))

(defn on-mouse-up [e]
  (.preventDefault e)
  (.stopPropagation e)
  (swap! drag assoc :active? false))

(defonce water-color "#1f77b4")

(def country-colors
  ["#9dc3c2"
   "#a7c8a0"
   "#c5ca91"
   "#e0cfa3"
   "#e1b6a0"
   "#d4a3a3"
   "#c4a3b5"
   "#b4a3c6"
   "#a3aad0"
   "#a3bfd8"
   "#92bccc"
   "#8fbfb8"
   "#a1c1a9"
   "#c2c2a3"
   "#d3b7a3"
   "#c6aba3"])

(rf/reg-event-db
  ::set-countries
  (fn [db [_ countries]]
    (let [colored (mapv #(assoc % :fill (rand-nth country-colors))
                        countries)]
      (assoc db
             :loading? false
             :countries colored))))

(rf/reg-event-fx
  ::initialize
  (fn [{:keys [db]} _]
    (if (:countries db)
      {:db db}
      {:db (assoc db
                  :loading? true
                  :projection :mercator)
       :dispatch [::load-countries]})))

(rf/reg-event-db
  ::set-projection
  (fn [db [_ projection]]
    (assoc db :projection projection)))

(rf/reg-event-fx
  ::load-countries
  (fn [_ _]
    {:fetch-countries "/countries.json"}))

(rf/reg-fx
  :fetch-countries
  (fn [url]
    (-> (js/fetch url)
        (.then
         (fn [resp]
           (if (.-ok resp)
             (.json resp)
             (throw (js/Error.
                     (str "Failed to load "
                          url
                          " ("
                          (.-status resp)
                          ")"))))))
        (.then
         (fn [data]
           (rf/dispatch
            [::set-countries
             (js->clj data :keywordize-keys true)])))
        (.catch
         (fn [err]
           (js/console.error "Failed to fetch countries:" err))))))

(rf/reg-sub ::countries
  (fn [db _]
    (:countries db)))

(rf/reg-sub ::projection
  (fn [db _]
    (:projection db)))

(rf/reg-sub ::loading?
  (fn [db _]
    (:loading? db)))

(defn deg->rad [d]
  (* d (/ Math/PI 180)))

(defn rad->deg [r]
  (* r (/ 180 Math/PI)))

(defn mercator-projection [lon lat width height]
  (let [delta (deg->rad lon)
        phi (deg->rad (max (min lat 85.0) -85.0))
        x (* (/ (+ delta Math/PI)
                (* 2 Math/PI))
             width)
        y (- (* (/ height
                   (* 2 Math/PI))
                (Math/log
                 (Math/tan
                  (+ (/ Math/PI 4)
                     (/ phi 2))))))]
    [x y]))

(defn equirectangular-projection [lon lat width height]
  [(* (/ (+ lon 180) 360) width)
   (- (* (/ (- 90 lat) 180) height) (/ height 2))])

(defn miller-projection [lon lat width height]
  (let [delta (deg->rad lon)
        phi (deg->rad (max (min lat 89.5) -89.5))
        x (* (/ (+ delta Math/PI)
                (* 2 Math/PI))
             width)
        y (- (* height
                0.3
                (Math/log
                 (Math/tan
                  (+ (/ Math/PI 4)
                     (* 0.4 phi))))))]
    [x y]))

(defn gall-peters-projection [lon lat width height]
  (let [phi (deg->rad lat)
        x (* width (/ (+ lon 180) 360))
        y (- (* (/ height (* 2 Math/PI))
                (* 2
                   (Math/sin phi))))]
    [x y]))

(def robinson-data
  ;; latitude (deg), X coefficient, Y coefficient
  [[0 1.0000 0.0000]
   [5 0.9986 0.0620]
   [10 0.9954 0.1240]
   [15 0.9900 0.1860]
   [20 0.9822 0.2480]
   [25 0.9730 0.3100]
   [30 0.9600 0.3720]
   [35 0.9427 0.4340]
   [40 0.9216 0.4958]
   [45 0.8962 0.5571]
   [50 0.8679 0.6176]
   [55 0.8350 0.6769]
   [60 0.7986 0.7346]
   [65 0.7597 0.7903]
   [70 0.7186 0.8435]
   [75 0.6732 0.8936]
   [80 0.6213 0.9394]
   [85 0.5722 0.9761]
   [90 0.5322 1.0000]])

(defn lerp [a b t] (+ a (* (- b a) t)))

(defn robinson-projection [lon lat width height]
  (let [abs-lat (js/Math.abs lat)
        entries robinson-data
        upper-idx (last (take-while #(> abs-lat (first %))
                                    entries))
        lower-idx (nth entries (min (+ (.indexOf entries upper-idx) 1)
                                    (dec (count entries))))
        [phi1 x1 y1] upper-idx
        [phi2 x2 y2] lower-idx
        t (/ (- abs-lat phi1) (- phi2 phi1))
        xcoef (lerp x1 x2 t)
        ycoef (lerp y1 y2 t)
        delta (deg->rad lon)
        sign (if (neg? lat) -1 1)
        r (/ width 2)]
    [(+ (/ width 2) (* r xcoef (/ delta Math/PI)))
     (- (* (/ height 2) sign ycoef))]))

(defn mollweide-projection [lon lat width height]
  (let [lambda (deg->rad lon)
        phi (deg->rad lat)
        epsilon 1e-10
        theta (loop [t phi
                     i 0]
                (let [f (- (+ (* 2 t) (js/Math.sin (* 2 t)))
                           (* Math/PI (js/Math.sin phi)))
                      f' (* 2 (+ 1 (js/Math.cos (* 2 t))))
                      delta (/ f f')]
                  (if (or (> i 30) (< (js/Math.abs delta) epsilon))
                    t
                    (recur (- t delta) (inc i)))))
        x (* (/ width 4)
             (/ (* 2) Math/PI)
             lambda
             (js/Math.cos theta))
        y (* (/ height 2)
             (js/Math.sin theta))]
    [(+ (/ width 2) x)
     (- y)]))

(defn eckert4-projection [lon lat width height]
  (let [lambda (deg->rad lon)
        phi (deg->rad lat)
        two-plus-piover2 (+ 2 (/ Math/PI 2))
        tolerance 1e-10
        ;; solve for theta : theta + sin(theta)*cos(theta) + 2*sin(theta)
        ;; = (2 + pi/2) * sin(phi)
        theta (loop [t phi
                     i 0]
                (let [f (- (+ t
                              (* (js/Math.sin t)
                                 (+ (js/Math.cos t) 2)))
                           (* two-plus-piover2
                              (js/Math.sin phi)))
                      fprime (+ 1
                                (* (js/Math.cos t)
                                   (+ (js/Math.cos t) 2))
                                (* (- (js/Math.sin t))
                                   (js/Math.sin t)))]
                  (if (or (> i 30)
                          (< (js/Math.abs f) tolerance))
                    t
                    (recur (- t (/ f fprime))
                           (inc i)))))
        xn (* 0.6
              lambda
              (+ 1 (js/Math.cos theta)))
        yn (* (js/Math.sin theta))
        sx (/ width 8)
        sy (/ height 2)]
    [(+ (/ width 2) (* sx xn))
     (- (* sy yn))]))

(defn country->paths [proj-fn country width height]
  (map-indexed
   (fn [pi poly]
     {:id pi
      :path (when (seq poly)
              (str "M "
                   (->> poly
                        (map (fn [[lon lat]]
                               (let [[x y] (proj-fn lon lat width height)]
                                 (str x "," y))))
                        (str/join " L "))
                   " Z"))})
   (:polygons country)))

(rf/reg-sub
  ::projected-paths
  :<- [::countries]
  (fn [countries [_ idx projection]]
    (let [c (get countries idx)
          proj-fn (case projection
                    :mercator mercator-projection
                    :equirectangular equirectangular-projection
                    :miller miller-projection
                    :gall-peters gall-peters-projection
                    :robinson robinson-projection
                    :mollweide mollweide-projection
                    :eckert4 eckert4-projection
                    mercator-projection)
          height (case projection
                   :equirectangular 500
                   :miller 750
                   :gall-peters 650
                   :robinson 500
                   :mollweide 500
                   :eckert4 500
                   1000)]
      (when c
        (country->paths proj-fn c 1000 height)))))

(defn country [{:keys [idx projection]}]
  (let [paths @(rf/subscribe [::projected-paths idx projection])
        country (get @(rf/subscribe [::countries])
                     idx)]
    [:<>
     (for [{:keys [id path]} paths]
       ^{:key (str "p-" idx "-" id)}
       [:path {:d path
               :stroke "#333"
               :strokeWidth 0.5
               :fill (:fill country)
               :vectorEffect "non-scaling-stroke"}])]))

(defn sidebar []
  (let [projection @(rf/subscribe [::projection])]
    [:div {:style {:display "flex"
                   :flexDirection "column"
                   :width "220px"
                   :padding "10px"
                   :backgroundColor "#f2f2f2"
                   :borderLeft "1px solid #ccc"}}
     [:label {:for "projection-select"
              :style {:marginBottom "8px"
                      :fontWeight "bold"}}
      "Projection:"]
     [:select {:id "projection-select"
               :value (name projection)
               :on-change #(rf/dispatch
                            [::set-projection
                             (keyword (.. % -target -value))])
               :style {:padding "4px"
                       :fontSize "14px"}}
      [:option {:value "mercator"} "Mercator"]
      [:option {:value "equirectangular"} "Equirectangular"]
      [:option {:value "miller"} "Miller"]
      [:option {:value "gall-peters"} "Gall–Peters"]
      [:option {:value "robinson"} "Robinson"]
      [:option {:value "mollweide"} "Mollweide"]
      [:option {:value "eckert4"} "Eckert IV"]]]))

(defn world []
  (let [svg-ref (ra/atom nil)]
    (ra/create-class
     {:component-did-mount
      (fn [_]
        (when-let [svg @svg-ref]
          (.addEventListener svg
                             "wheel"
                             (fn [e] (handle-wheel svg e))
                             #js {:passive false})))
      :reagent-render
      (fn []
        (let [countries @(rf/subscribe [::countries])
              projection @(rf/subscribe [::projection])]
          [:div {:style {:display "flex"
                         :flexDirection "row"
                         :height "100vh"
                         :overflow "hidden"}}
           [:svg {:ref #(reset! svg-ref %)
                  :viewBox "0 -500 1000 1000"
                  :style {:flex "1"
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
                     :y -500
                     :width 1000
                     :height 1000
                     :fill water-color}]
             (for [i (range (count countries))]
               ^{:key i}
               [country {:idx i
                         :projection projection}])]]
           [sidebar]]))})))

(defonce ^:dynamic *app-root*
  nil)

(defn app-root []
  (when *app-root*
    (r/unmount *app-root*))
  (set! *app-root*
        (r/create-root
         (.getElementById js/document "app")))
  *app-root*)

(defn app []
  (let [loading? @(rf/subscribe [::loading?])]
    (if loading?
      [:p "Loading..."]
      [world])))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (r/render (app-root) [app]))
