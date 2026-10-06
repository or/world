(ns world.core
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.core :as ra]
   [reagent.dom.client :as r]
   [world.clip :as clip]))

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

(def resolutions
  ;; Natural Earth scales, see src/prepare-data.py
  [[:110m "Low (1:110m)"]
   [:50m "Medium (1:50m)"]
   [:10m "High (1:10m)"]])

(defn country-fill [name]
  ;; Stable per country, so it doesn't change when switching resolutions.
  (nth country-colors (mod (hash name) (count country-colors))))

(defn parse-countries [data]
  ;; Coordinates stay plain JS arrays: converting hundreds of thousands of
  ;; points with js->clj is slow, and nothing needs them as persistent data.
  (mapv (fn [^js c]
          {:name (.-name c)
           :fill (country-fill (.-name c))
           :label-position (vec (.-label_position c))
           :polygons (.-polygons c)
           :bounds (mapv clip/bounds (.-polygons c))})
        data))

(rf/reg-event-db
  ::set-countries
  (fn [db [_ resolution countries]]
    (cond-> (-> db
                (assoc-in [:datasets resolution] countries)
                (update :loading disj resolution))
      (= resolution (:resolution db))
      (assoc :shown-resolution resolution))))

(rf/reg-event-fx
  ::initialize
  (fn [{:keys [db]} _]
    (if (:resolution db)
      {:db db}
      {:db (assoc db
                  :projection :equal-earth
                  :central-meridian 0
                  :datasets {}
                  :loading #{})
       :dispatch [::set-resolution :50m]})))

(rf/reg-event-fx
  ::set-resolution
  (fn [{:keys [db]} [_ resolution]]
    (let [db (assoc db :resolution resolution)]
      (cond
        (get-in db [:datasets resolution])
        {:db (assoc db :shown-resolution resolution)}

        (contains? (:loading db) resolution)
        {:db db}

        :else
        {:db (update db :loading conj resolution)
         :fetch-countries resolution}))))

(rf/reg-fx
  :fetch-countries
  (fn [resolution]
    (let [url (str "/countries-" (name resolution) ".json")]
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
              [::set-countries resolution (parse-countries data)])))
          (.catch
           (fn [err]
             (js/console.error "Failed to fetch countries:" err)))))))

(def preview-resolution
  :110m)

(rf/reg-sub ::shown-resolution
  ;; Dragging the meridian slider re-projects everything on every step, which
  ;; is too slow for smooth dragging with detailed boundaries.
  (fn [{:keys [shown-resolution dragging-meridian? datasets]} _]
    (if (and dragging-meridian? (get datasets preview-resolution))
      preview-resolution
      shown-resolution)))

(rf/reg-sub ::datasets
  (fn [db _]
    (:datasets db)))

(rf/reg-sub ::resolution
  (fn [db _]
    (:resolution db)))

(rf/reg-sub ::countries
  :<- [::shown-resolution]
  :<- [::datasets]
  (fn [[resolution datasets] _]
    (get datasets resolution)))

(rf/reg-sub ::projection
  (fn [db _]
    (:projection db)))

(rf/reg-sub ::central-meridian
  (fn [db _]
    (:central-meridian db 0)))

(rf/reg-sub ::south-up?
  (fn [db _]
    (:south-up? db)))

(rf/reg-sub ::loading?
  (fn [db _]
    (nil? (:shown-resolution db))))

(rf/reg-sub ::fetching?
  (fn [db _]
    (boolean (seq (:loading db)))))

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
  (let [abs-lat (min (js/Math.abs lat) 90)
        i (min (js/Math.floor (/ abs-lat 5))
               (- (count robinson-data) 2))
        [phi1 x1 y1] (nth robinson-data i)
        [_ x2 y2] (nth robinson-data (inc i))
        t (/ (- abs-lat phi1) 5)
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
        ;; At the poles theta = phi, and Newton would divide by f' = 0.
        theta (if (> (js/Math.abs phi) (- (/ Math/PI 2) epsilon))
                phi
                (loop [t phi
                       i 0]
                  (let [f (- (+ (* 2 t) (js/Math.sin (* 2 t)))
                             (* Math/PI (js/Math.sin phi)))
                        f' (* 2 (+ 1 (js/Math.cos (* 2 t))))
                        delta (/ f f')]
                    (if (or (> i 30) (< (js/Math.abs delta) epsilon))
                      t
                      (recur (- t delta) (inc i))))))
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

(defn round2 [x]
  (/ (js/Math.round (* x 100)) 100))

;; Šavrič, Patterson & Jenny (2018): "The Equal Earth map projection"
(def equal-earth-a1 1.340264)
(def equal-earth-a2 -0.081106)
(def equal-earth-a3 0.000893)
(def equal-earth-a4 0.003796)
(def equal-earth-m (/ (js/Math.sqrt 3) 2))

(defn equal-earth-unscaled [lambda phi]
  (let [theta (js/Math.asin (* equal-earth-m (js/Math.sin phi)))
        t2 (* theta theta)
        t6 (* t2 t2 t2)
        x (/ (* 2 (js/Math.sqrt 3) lambda (js/Math.cos theta))
             (* 3 (+ equal-earth-a1
                     (* 3 equal-earth-a2 t2)
                     (* t6 (+ (* 7 equal-earth-a3)
                              (* 9 equal-earth-a4 t2))))))
        y (* theta
             (+ equal-earth-a1
                (* equal-earth-a2 t2)
                (* t6 (+ equal-earth-a3
                         (* equal-earth-a4 t2)))))]
    [x y]))

;; Extent of the unscaled map: x at (180°, 0°), y at the pole
(def equal-earth-max-x
  (first (equal-earth-unscaled Math/PI 0)))

(def equal-earth-max-y
  (second (equal-earth-unscaled 0 (/ Math/PI 2))))

(defn equal-earth-projection [lon lat width height]
  (let [[x y] (equal-earth-unscaled (deg->rad lon) (deg->rad lat))]
    [(+ (/ width 2) (* (/ width 2 equal-earth-max-x) x))
     (- (* (/ height 2 equal-earth-max-y) y))]))

(defn push-path! [^js out proj-fn ^js points offset width height close?]
  (dotimes [i (alength points)]
    (let [^js point (aget points i)
          [x y] (proj-fn (+ (aget point 0) offset) (aget point 1) width height)]
      (.push out (if (zero? i) "M" "L") (round2 x) "," (round2 y))))
  (when close?
    (.push out "Z")))

(defn country->paths
  "SVG paths [fill outline] for a country on a map centered on longitude
  `center`. The fill has all polygons, holes included (fill-rule evenodd)."
  [proj-fn country center width height]
  (let [fill #js []
        outline #js []]
    (doseq [[polygon bounds] (map vector (:polygons country) (:bounds country))
            strip (clip/strips bounds center)
            :let [[offset] strip]]
      (doseq [ring (clip/fill-rings polygon strip)]
        (push-path! fill proj-fn ring offset width height true))
      (doseq [ring polygon
              line (clip/outline-lines ring strip)]
        (push-path! outline proj-fn line offset width height false)))
    [(.join fill "") (.join outline "")]))

(def projections
  {:mercator [mercator-projection 1000]
   :equirectangular [equirectangular-projection 500]
   :miller [miller-projection 750]
   :gall-peters [gall-peters-projection 650]
   :robinson [robinson-projection 500]
   :mollweide [mollweide-projection 500]
   :eckert4 [eckert4-projection 500]
   ;; equal-earth-max-x / equal-earth-max-y ≈ 2.05
   :equal-earth [equal-earth-projection 487]})

(defn get-projection [projection]
  (get projections projection (:mercator projections)))

(defn unproject
  "Inverse of proj-fn: map coordinates -> [lon lat]. Found numerically, which
  works for all projections here, because y depends only on latitude (and
  decreases as it grows), and for a fixed latitude x is linear in longitude."
  [proj-fn x y width height]
  (let [lat (loop [lo -90
                   hi 90
                   i 0]
              (let [mid (/ (+ lo hi) 2)]
                (cond
                  (= i 50) mid
                  (> (second (proj-fn 0 mid width height)) y) (recur mid hi (inc i))
                  :else (recur lo mid (inc i)))))
        [x0] (proj-fn 0 lat width height)
        [x180] (proj-fn 180 lat width height)
        lon (if (== x0 x180)
              0
              (* 180 (/ (- x x0) (- x180 x0))))]
    [(clamp lon -180 180) lat]))

;; Center of the SVG's viewBox, which is always the center of the viewport.
;; It's also the center of every projection (lon 0, lat 0), so the map can be
;; rotated around it.
(def view-center
  [500 0])

(defn rotate-south-up
  "Map coordinates <-> coordinates as shown, rotated 180° when south-up?.
  The rotation is its own inverse, so this works in both directions."
  [south-up? [x y]]
  (let [[cx cy] view-center]
    (if south-up?
      [(- (* 2 cx) x) (- (* 2 cy) y)]
      [x y])))

(defn view-settings [db]
  (select-keys db [:projection :south-up?]))

(rf/reg-fx
  ::keep-center
  ;; Pan so that the lon/lat at the center of the view stays there, when
  ;; switching between view settings (projection, rotation).
  (fn [[from to]]
    (let [[cx cy] view-center
          s @zoom
          [tx ty] @translate
          [from-fn from-height] (get-projection (:projection from))
          [to-fn to-height] (get-projection (:projection to))
          [fx fy] (rotate-south-up (:south-up? from)
                                   [(/ (- cx tx) s)
                                    (/ (- cy ty) s)])
          [lon lat] (unproject from-fn fx fy 1000 from-height)
          [x y] (rotate-south-up (:south-up? to)
                                 (to-fn lon lat 1000 to-height))]
      (reset! translate [(- cx (* s x))
                         (- cy (* s y))]))))

(rf/reg-event-fx
  ::set-projection
  (fn [{:keys [db]} [_ projection]]
    (let [new-db (assoc db :projection projection)]
      {:db new-db
       ::keep-center [(view-settings db) (view-settings new-db)]})))

(rf/reg-fx
  ::center-meridian
  ;; Pan horizontally so the central meridian is on the center line of the
  ;; view. It's the vertical line through the middle of the map, also when
  ;; rotated.
  (fn [_]
    (let [[cx] view-center
          [_ ty] @translate]
      (reset! translate [(- cx (* @zoom cx)) ty]))))

(rf/reg-event-fx
  ::set-central-meridian
  (fn [{:keys [db]} [_ central-meridian]]
    {:db (assoc db :central-meridian central-meridian)
     ::center-meridian nil}))

(rf/reg-event-fx
  ::set-dragging-meridian
  (fn [{:keys [db]} [_ dragging?]]
    (let [db (assoc db :dragging-meridian? dragging?)]
      (if (and dragging?
               (not (get-in db [:datasets preview-resolution]))
               (not (contains? (:loading db) preview-resolution)))
        {:db (update db :loading conj preview-resolution)
         :fetch-countries preview-resolution}
        {:db db}))))

(rf/reg-event-fx
  ::set-south-up
  (fn [{:keys [db]} [_ south-up?]]
    (let [new-db (assoc db :south-up? south-up?)]
      {:db new-db
       ::keep-center [(view-settings db) (view-settings new-db)]})))

;; [resolution projection central-meridian] -> vector of [fill outline]
;; paths. Projecting the 10m data takes a moment, so switching back to a view
;; seen before is instant. Only the most recent ones are kept, as dragging the
;; meridian slider creates lots.
;; Plain def (not defonce), so it's cleared when this file is hot-reloaded.
(def path-cache
  (atom {:keys []
         :paths {}}))

(def path-cache-size
  12)

(defn cache-paths! [k paths]
  (swap! path-cache
         (fn [{:keys [keys] :as cache}]
           (let [evicted (drop path-cache-size (cons k (reverse keys)))]
             {:keys (conj (vec (remove (set evicted) keys)) k)
              :paths (-> (apply dissoc (:paths cache) evicted)
                         (assoc k paths))})))
  paths)

(rf/reg-sub
  ::projected-paths
  :<- [::shown-resolution]
  :<- [::countries]
  :<- [::projection]
  :<- [::central-meridian]
  (fn [[resolution countries projection central-meridian] _]
    (let [k [resolution projection central-meridian]]
      (or (get-in @path-cache [:paths k])
          (let [[proj-fn height] (get-projection projection)]
            (cache-paths! k (mapv #(country->paths proj-fn
                                                   %
                                                   central-meridian
                                                   1000
                                                   height)
                                  countries)))))))

(defn countries-layer []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])]
    ;; All outlines on top of all fills, so no fill covers a neighbour's
    ;; border.
    [:<>
     [:g
      (for [[i {:keys [fill]}] (map-indexed vector countries)]
        ^{:key i}
        [:path {:d (first (nth paths i))
                :fill fill
                :fillRule "evenodd"}])]
     [:g {:stroke "#333"
          :strokeWidth 0.5
          :fill "none"}
      (for [i (range (count countries))]
        ^{:key i}
        [:path {:d (second (nth paths i))
                :vectorEffect "non-scaling-stroke"}])]]))

(defn format-longitude [lon]
  (cond
    (or (zero? lon) (== 180 (js/Math.abs lon))) (str (js/Math.abs lon) "°")
    (pos? lon) (str lon "°E")
    :else (str (- lon) "°W")))

(defn sidebar []
  (let [projection @(rf/subscribe [::projection])
        resolution @(rf/subscribe [::resolution])
        fetching? @(rf/subscribe [::fetching?])
        south-up? @(rf/subscribe [::south-up?])
        central-meridian @(rf/subscribe [::central-meridian])]
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
      [:option {:value "equal-earth"} "Equal Earth"]
      [:option {:value "mercator"} "Mercator"]
      [:option {:value "equirectangular"} "Equirectangular"]
      [:option {:value "miller"} "Miller"]
      [:option {:value "gall-peters"} "Gall–Peters"]
      [:option {:value "robinson"} "Robinson"]
      [:option {:value "mollweide"} "Mollweide"]
      [:option {:value "eckert4"} "Eckert IV"]]
     [:label {:style {:marginTop "8px"}}
      [:input {:type "checkbox"
               :checked (boolean south-up?)
               :on-change #(rf/dispatch
                            [::set-south-up
                             (.. % -target -checked)])}]
      " South up"]
     [:label {:for "meridian-slider"
              :style {:marginTop "16px"
                      :marginBottom "8px"
                      :fontWeight "bold"}}
      "Central meridian: " (format-longitude central-meridian)]
     [:input {:id "meridian-slider"
              :type "range"
              :min -180
              :max 180
              :step 1
              :value central-meridian
              :on-pointer-down (fn [_]
                                 (rf/dispatch [::set-dragging-meridian true])
                                 ;; on window, as the pointer may be released
                                 ;; outside the slider
                                 (.addEventListener
                                  js/window
                                  "pointerup"
                                  #(rf/dispatch [::set-dragging-meridian false])
                                  #js {:once true}))
              :on-change #(rf/dispatch
                           [::set-central-meridian
                            (js/parseInt (.. % -target -value))])}]
     [:label {:for "resolution-select"
              :style {:marginTop "16px"
                      :marginBottom "8px"
                      :fontWeight "bold"}}
      "Boundaries:"]
     [:select {:id "resolution-select"
               :value (name resolution)
               :on-change #(rf/dispatch
                            [::set-resolution
                             (keyword (.. % -target -value))])
               :style {:padding "4px"
                       :fontSize "14px"}}
      (for [[k label] resolutions]
        ^{:key k}
        [:option {:value (name k)} label])]
     (when fetching?
       [:p {:style {:color "#666"}} "Loading…"])]))

(defn rotated-layer []
  (let [south-up? @(rf/subscribe [::south-up?])
        [cx cy] view-center]
    [:g {:transform (when south-up?
                      (str "rotate(180 " cx " " cy ")"))}
     [countries-layer]]))

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
           [rotated-layer]]]
         [sidebar]])})))

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
