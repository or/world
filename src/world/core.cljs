(ns world.core
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.core :as ra]
   [reagent.dom.client :as r]
   [world.clip :as clip]
   [world.grid :as grid]
   [world.projection :as proj]))

(defonce zoom
  (ra/atom 1.0))

(defonce translate
  (ra/atom [0 0]))

;; Size of the SVG element in pixels, for what part of the viewBox is visible.
(defonce svg-size
  (ra/atom [1000 1000]))

;; Name of the country under the mouse. By name, as countries' indices
;; differ between resolutions.
(defonce hovered
  (ra/atom nil))

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

(def resolutions
  ;; Natural Earth scales, see src/prepare-data.py
  [[:110m "Low (1:110m)"]
   [:50m "Medium (1:50m)"]
   [:10m "High (1:10m)"]])

(defn parse-countries [data]
  ;; Coordinates stay plain JS arrays: converting hundreds of thousands of
  ;; points with js->clj is slow, and nothing needs them as persistent data.
  (mapv (fn [^js c]
          {:name (.-name c)
           :long-name (.-long_name c)
           :iso-a2 (.-iso_a2 c)
           :capital (.-capital c)
           :population (.-population c)
           :population-year (.-population_year c)
           :area (.-area c)
           ;; assigned in prepare-data.py, different from neighbours'
           :fill (.-fill c)
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

(def default-grid-settings
  {:grid? true
   :step :auto
   :equator? true
   :prime-meridian? true
   :labels? true})

(rf/reg-sub ::grid-settings
  (fn [db _]
    (merge default-grid-settings (:grid db))))

(rf/reg-event-db
  ::set-grid-setting
  (fn [db [_ k v]]
    (assoc-in db [:grid k] v)))

(rf/reg-sub ::south-up?
  (fn [db _]
    (:south-up? db)))

(rf/reg-sub ::loading?
  (fn [db _]
    (nil? (:shown-resolution db))))

(rf/reg-sub ::fetching?
  (fn [db _]
    (boolean (seq (:loading db)))))

(defn round2 [x]
  (/ (js/Math.round (* x 100)) 100))

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
          [from-fn from-height] (proj/get-projection (:projection from))
          [to-fn to-height] (proj/get-projection (:projection to))
          [fx fy] (rotate-south-up (:south-up? from)
                                   [(/ (- cx tx) s)
                                    (/ (- cy ty) s)])
          [lon lat] (proj/unproject from-fn fx fy 1000 from-height)
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
          (let [[proj-fn height] (proj/get-projection projection)]
            (cache-paths! k (mapv #(country->paths proj-fn
                                                   %
                                                   central-meridian
                                                   1000
                                                   height)
                                  countries)))))))

(def halo-width
  ;; pixels
  12)

(defn halos-layer
  "Invisible margins around countries that also count as hovering them, to
  make small islands easier to hover. Below the fills, so the country under
  the mouse wins. Smaller countries on top, so their margins win."
  []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])]
    [:g {:fill "none"
         :stroke "black"
         :strokeWidth halo-width
         :strokeLinejoin "round"
         ;; not drawn, but still hit by the mouse
         :visibility "hidden"
         :pointerEvents "stroke"}
     (for [[i {:keys [name]}] (sort-by (comp - :area second)
                                       (map-indexed vector countries))]
       ^{:key i}
       [:path {:d (first (nth paths i))
               :data-name name
               :vectorEffect "non-scaling-stroke"}])]))

(defn fills-layer []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])]
    [:g
     (for [[i {:keys [name fill]}] (map-indexed vector countries)]
       ^{:key i}
       [:path {:d (first (nth paths i))
               :data-name name
               :fill fill
               :fillRule "evenodd"}])]))

;; Outlines go on top of all fills (and the grid), so no fill covers a
;; neighbour's border.
(defn outlines-layer []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])]
    [:g {:stroke "#333"
         :strokeWidth 0.5
         :fill "none"
         :pointerEvents "none"}
     (for [i (range (count countries))]
       ^{:key i}
       [:path {:d (second (nth paths i))
               :vectorEffect "non-scaling-stroke"}])]))

(defn highlight-layer []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])
        name @hovered
        i (first (keep-indexed #(when (= name (:name %2)) %1) countries))]
    (when i
      [:path {:d (second (nth paths i))
              :stroke "#111"
              :strokeWidth 2
              :fill "none"
              :pointerEvents "none"
              :vectorEffect "non-scaling-stroke"}])))

(defn grid-step [step]
  (if (= step :auto)
    (grid/auto-step @zoom)
    step))

(rf/reg-sub
  ::grid-path
  :<- [::projection]
  :<- [::central-meridian]
  (fn [[projection center] [_ step]]
    (grid/grid-path projection center step)))

(rf/reg-sub
  ::equator-path
  :<- [::projection]
  (fn [projection _]
    (grid/equator-path projection)))

(rf/reg-sub
  ::prime-meridian-path
  :<- [::projection]
  :<- [::central-meridian]
  (fn [[projection center] _]
    (grid/prime-meridian-path projection center)))

(def thick-line
  {:stroke "rgba(255, 255, 255, 0.9)"
   :strokeWidth 1.5
   :vectorEffect "non-scaling-stroke"})

(defn grid-layer []
  (let [{:keys [grid? step equator? prime-meridian?]}
        @(rf/subscribe [::grid-settings])]
    [:g {:fill "none"
         :pointerEvents "none"}
     (when grid?
       [:path {:d @(rf/subscribe [::grid-path (grid-step step)])
               :stroke "rgba(255, 255, 255, 0.5)"
               :strokeWidth 0.5
               :vectorEffect "non-scaling-stroke"}])
     (when equator?
       [:path (assoc thick-line :d @(rf/subscribe [::equator-path]))])
     (when prime-meridian?
       [:path (assoc thick-line :d @(rf/subscribe [::prime-meridian-path]))])]))

(defn visible-area
  "The part of the viewBox that's visible: [left top right bottom]. The SVG
  scales the viewBox to fit and centers it."
  []
  (let [[w h] @svg-size
        [cx cy] view-center
        k (min (/ w 1000) (/ h 1000))
        half-w (/ w k 2)
        half-h (/ h k 2)]
    [(- cx half-w) (- cy half-h) (+ cx half-w) (+ cy half-h)]))

(defn grid-labels-layer []
  (let [{:keys [grid? step equator? prime-meridian? labels?]}
        @(rf/subscribe [::grid-settings])
        projection @(rf/subscribe [::projection])
        center @(rf/subscribe [::central-meridian])
        south-up? @(rf/subscribe [::south-up?])
        s @zoom
        [tx ty] @translate]
    (when (and labels? (or grid? equator? prime-meridian?))
      [:g {:fontSize grid/font-size
           :fill "#222"
           :stroke "rgba(255, 255, 255, 0.8)"
           :strokeWidth 3
           :strokeLinejoin "round"
           :paintOrder "stroke"
           :pointerEvents "none"
           :style {:userSelect "none"}}
       (for [{:keys [key x y text anchor baseline]}
             (grid/labels
              {:projection projection
               :center center
               :step (grid-step step)
               :grid? grid?
               :equator? equator?
               :prime-meridian? prime-meridian?
               :to-screen (fn [p]
                            (let [[x y] (rotate-south-up south-up? p)]
                              [(+ tx (* s x)) (+ ty (* s y))]))
               :from-screen (fn [[x y]]
                              (rotate-south-up south-up?
                                               [(/ (- x tx) s)
                                                (/ (- y ty) s)]))
               :visible (visible-area)})]
         ^{:key key}
         [:text {:x x
                 :y y
                 :textAnchor anchor
                 :dominantBaseline baseline}
          text])])))

(defn checkbox [label checked? on-change]
  [:label {:style {:marginTop "8px"}}
   [:input {:type "checkbox"
            :checked (boolean checked?)
            :on-change #(on-change (.. % -target -checked))}]
   " " label])

(defn grid-settings []
  (let [{:keys [grid? step equator? prime-meridian? labels?]}
        @(rf/subscribe [::grid-settings])
        set-setting #(rf/dispatch [::set-grid-setting %1 %2])]
    [:<>
     [:div {:style {:marginTop "16px"
                    :fontWeight "bold"}}
      "Grid:"]
     [:div {:style {:display "flex"
                    :alignItems "center"
                    :gap "8px"}}
      [checkbox "Lines every" grid? #(set-setting :grid? %)]
      [:select {:value (if (= step :auto) "auto" (str step))
                :disabled (not grid?)
                :on-change #(let [v (.. % -target -value)]
                              (set-setting :step (if (= v "auto")
                                                   :auto
                                                   (js/parseInt v))))
                :style {:marginTop "8px"
                        :padding "2px"
                        :fontSize "14px"}}
       [:option {:value "auto"} "auto"]
       (for [step (reverse grid/steps)]
         ^{:key step}
         [:option {:value (str step)} (str step "°")])]]
     [checkbox "Equator" equator? #(set-setting :equator? %)]
     [checkbox "Prime meridian" prime-meridian? #(set-setting :prime-meridian? %)]
     [checkbox "Labels" labels? #(set-setting :labels? %)]]))

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
      "Central meridian: " (grid/format-longitude central-meridian)]
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
       [:p {:style {:color "#666"}} "Loading…"])
     [grid-settings]]))

(defn rotated-layer []
  (let [south-up? @(rf/subscribe [::south-up?])
        [cx cy] view-center]
    [:g {:transform (when south-up?
                      (str "rotate(180 " cx " " cy ")"))}
     [:g {:on-mouse-over #(reset! hovered (.. % -target -dataset -name))
          :on-mouse-leave #(reset! hovered nil)}
      [halos-layer]
      [fills-layer]]
     [grid-layer]
     [outlines-layer]
     [highlight-layer]]))

(defn flag-emoji
  "The flag for a two-letter country code, from regional indicator symbols."
  [iso-a2]
  (apply str (map #(js/String.fromCodePoint (+ 0x1F1E6 (- (.charCodeAt % 0) 65)))
                  iso-a2)))

(defn format-population [n]
  (cond
    (>= n 1e9) (str (.toFixed (/ n 1e9) 2) " billion")
    (>= n 1e6) (str (.toFixed (/ n 1e6) 1) " million")
    :else (.toLocaleString n "en")))

(defn country-info []
  (let [countries @(rf/subscribe [::countries])
        name @hovered
        {:keys [long-name iso-a2 capital population population-year]}
        (first (filter #(= name (:name %)) countries))]
    (when long-name
      [:div {:style {:position "absolute"
                     :right "16px"
                     :bottom "16px"
                     :minWidth "220px"
                     :maxWidth "320px"
                     :padding "10px 14px"
                     :background "rgba(255, 255, 255, 0.95)"
                     :borderRadius "6px"
                     :boxShadow "0 2px 8px rgba(0, 0, 0, 0.3)"
                     :fontSize "14px"
                     :lineHeight "1.5"
                     :pointerEvents "none"}}
       [:div {:style {:display "flex"
                      :justifyContent "space-between"
                      :alignItems "flex-start"
                      :gap "12px"}}
        [:div {:style {:fontSize "17px"
                       :fontWeight "bold"}}
         long-name]
        (when iso-a2
          [:div {:style {:fontSize "28px"
                         :lineHeight "1"}}
           (flag-emoji iso-a2)])]
       (when capital
         [:div "Capital: " capital])
       (when population
         [:div "Population: " (format-population population)
          [:span {:style {:color "#777"}} " (" population-year ")"]])])))

;; Only once per page load, not on hot reloads.
(defonce initial-view-set?
  (atom false))

(defn fit-width!
  "Zoom so the map fills the width of the view, centered. The map is as wide
  as the viewBox, which fills the height of the view when it's wider than
  tall."
  [[w h]]
  (let [[cx cy] view-center
        s (clamp (/ w h) min-zoom max-zoom)]
    (reset! zoom s)
    (reset! translate [(- cx (* s cx)) (- cy (* s cy))])))

(defn world []
  (let [svg-ref (ra/atom nil)]
    (ra/create-class
     {:component-did-mount
      (fn [_]
        (when-let [svg @svg-ref]
          (.addEventListener svg
                             "wheel"
                             (fn [e] (handle-wheel svg e))
                             #js {:passive false})
          (.observe (js/ResizeObserver.
                     (fn [entries]
                       (let [rect (.-contentRect (aget entries 0))
                             size [(.-width rect) (.-height rect)]]
                         (reset! svg-size size)
                         (when-not @initial-view-set?
                           (reset! initial-view-set? true)
                           (fit-width! size)))))
                    svg)))
      :reagent-render
      (fn []
        [:div {:style {:display "flex"
                       :flexDirection "row"
                       :height "100vh"
                       :overflow "hidden"}}
         [:div {:style {:flex "1"
                        :position "relative"
                        :minWidth 0}}
          [:svg {:ref #(reset! svg-ref %)
                 :viewBox "0 -500 1000 1000"
                 :style {:display "block"
                         :width "100%"
                         :height "100%"
                         ;; an arrow is better for pointing at countries
                         :cursor (if (:active? @drag)
                                   "grabbing"
                                   "default")}
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
            [rotated-layer]]
           [grid-labels-layer]]
          [country-info]]
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
