(ns world.core
  (:require
   [cljs.reader :as reader]
   [clojure.string :as str]
   [re-frame.core :as rf]
   [re-frame.db]
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
  ;; In viewBox coordinates, like translate: the SVG scales them to the
  ;; screen.
  (reset! drag
          {:active? true
           :start (svg-coords (.-currentTarget e) e)
           :base @translate}))

(defn on-mouse-move [e]
  (when (:active? @drag)
    (.preventDefault e)
    (.stopPropagation e)
    (let [{:keys [start base]} @drag
          [sx sy] start
          [bx by] base
          [x y] (svg-coords (.-currentTarget e) e)
          dx (- x sx)
          dy (- y sy)]
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
           ;; e.g. "France" for French Guiana, which is shown separately
           :part-of (.-part_of c)
           ;; the country's code, the same for all its parts
           :group (.-group c)
           :capitals (mapv (fn [^js capital]
                             {:name (.-name capital)
                              :note (.-note capital)
                              :lat (.-lat capital)
                              :lon (.-lon capital)})
                           (.-capitals c))
           :population (.-population c)
           :population-year (.-population_year c)
           ;; where to put the name, and from which zoom level on
           :label (.-label c)
           :min-label (.-min_label c)
           :area (.-area c)
           ;; assigned in prepare-data.py, different from neighbours'
           :fill (.-fill c)
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
  ;; The settings saved last time, see ::saved-settings
  [(rf/inject-cofx ::saved-settings)]
  (fn [{:keys [db saved-settings]} _]
    (if (:resolution db)
      {:db db}
      {:db (merge (assoc db
                         :projection :equal-earth
                         :central-meridian 0
                         :datasets {}
                         :loading #{})
                  (select-keys saved-settings
                               [:projection
                                :south-up?
                                :central-meridian
                                :country-names?
                                :grid]))
       :dispatch [::set-resolution (:resolution saved-settings :50m)]})))

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
    ;; relative, so the app works in a subdirectory, e.g. on GitHub Pages
    (let [url (str "countries-" (name resolution) ".json")]
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

(rf/reg-sub ::country-names?
  (fn [db _]
    (:country-names? db true)))

(rf/reg-event-db
  ::set-country-names
  (fn [db [_ country-names?]]
    (assoc db :country-names? country-names?)))

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

(defn highlight-layer
  "Outlines the hovered country, and less boldly the other parts of the same
  country, e.g. France for French Guiana and vice versa."
  []
  (let [countries @(rf/subscribe [::countries])
        paths @(rf/subscribe [::projected-paths])
        name @hovered
        group (:group (first (filter #(= name (:name %)) countries)))]
    (when group
      [:g {:stroke "#111"
           :fill "none"
           :pointerEvents "none"}
       ;; the hovered one last, on top
       (for [[i country] (sort-by #(= name (:name (second %)))
                                  (keep-indexed #(when (= group (:group %2))
                                                   [%1 %2])
                                                countries))]
         ^{:key i}
         [:path {:d (second (nth paths i))
                 :strokeWidth (if (= name (:name country)) 2 1.25)
                 :vectorEffect "non-scaling-stroke"}])])))

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

(defn current-view
  "What's needed to convert between map and screen (viewBox) coordinates.
  Deref'd here, while rendering, so components using it re-render when the
  view changes: in to-screen and from-screen, that may be too late, e.g. in
  lazy sequences."
  [south-up?]
  {:south-up? south-up?
   :zoom @zoom
   :translate @translate})

(defn to-screen
  "Map coordinates -> screen (viewBox) coordinates."
  [{:keys [south-up? zoom translate]} p]
  (let [[x y] (rotate-south-up south-up? p)
        [tx ty] translate]
    [(+ tx (* zoom x)) (+ ty (* zoom y))]))

(defn from-screen
  "Screen (viewBox) coordinates -> map coordinates."
  [{:keys [south-up? zoom translate]} [x y]]
  (let [[tx ty] translate]
    (rotate-south-up south-up? [(/ (- x tx) zoom) (/ (- y ty) zoom)])))

(defn grid-labels-layer []
  (let [{:keys [grid? step equator? prime-meridian? labels?]}
        @(rf/subscribe [::grid-settings])
        projection @(rf/subscribe [::projection])
        center @(rf/subscribe [::central-meridian])
        view (current-view @(rf/subscribe [::south-up?]))]
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
               :to-screen (partial to-screen view)
               :from-screen (partial from-screen view)
               :visible (visible-area)})]
         ^{:key key}
         [:text {:x x
                 :y y
                 :textAnchor anchor
                 :dominantBaseline baseline}
          text])])))

(defn hovered-country []
  (let [countries @(rf/subscribe [::countries])
        name @hovered]
    (first (filter #(= name (:name %)) countries))))

(defn lon-lat->screen-fn
  "A function from lon, lat to screen (viewBox) coordinates in the current
  view. Call it while rendering, like current-view."
  []
  (let [projection @(rf/subscribe [::projection])
        center @(rf/subscribe [::central-meridian])
        view (current-view @(rf/subscribe [::south-up?]))
        [proj-fn height] (proj/get-projection projection)]
    (fn [lon lat]
      (to-screen view
                 (proj-fn (grid/normalize-lon (- lon center)) lat 1000 height)))))

(defn hovered-capitals
  "The hovered country's capitals, where they are on the screen."
  [lon-lat->screen]
  (vec (for [{:keys [lat lon] :as capital} (:capitals (hovered-country))
             :let [[x y] (lon-lat->screen lon lat)]]
         (assoc capital :x x :y y))))

(def capital-font-size
  13)

(defn text-width
  "Roughly: characters are about 0.6 em wide on average."
  [font-size text]
  (* 0.6 font-size (count text)))

(defn capital-boxes
  "Where capital markers and their labels are, as [left top right bottom]."
  [capitals]
  (for [{:keys [x y name note]} capitals]
    [(- x 13)
     (- y 13)
     (+ x 15 (text-width capital-font-size (str name (when note (str " (" note ")")))))
     (+ y 13)]))

(defn capital-marker [x y name note]
  ;; a cross in a circle, dark with a white halo to stand out on any colour
  (let [shape [:<>
               [:circle {:r 7}]
               [:path {:d "M-12,0H12M0,-12V12"}]]]
    [:g {:transform (str "translate(" x " " y ")")}
     [:g {:fill "none"
          :stroke "white"
          :strokeWidth 4.5}
      shape]
     [:g {:fill "none"
          :stroke "#111"
          :strokeWidth 1.75}
      shape]
     [:text {:x 15
             :y 0
             :dominantBaseline "middle"
             :fontSize capital-font-size
             :fontWeight "bold"
             :fill "#111"
             :stroke "white"
             :strokeWidth 3.5
             :strokeLinejoin "round"
             :paintOrder "stroke"}
      name
      (when note
        [:tspan {:fontWeight "normal"} " (" note ")"])]]))

(defn web-zoom
  "The zoom level of a web map (OpenStreetMap etc.) at the same scale, which
  is what Natural Earth's label levels are for: at level z, the world is
  256 × 2^z pixels wide."
  []
  (let [[w h] @svg-size
        ;; pixels per viewBox unit
        scale (/ (min w h) 1000)]
    (js/Math.log2 (/ (* 1000 @zoom scale) 256))))

(defn without-overlaps
  "The labels that fit without overlapping more important ones before them, or
  obstacles: [{:x :y :font-size :text}], centered on x, y. A label can have
  :offsets, vertical ones to try in turn, and be :forced, placed at the first
  even if it doesn't fit."
  [labels obstacles]
  (let [box (fn [{:keys [x y font-size text]}]
              (let [half-w (/ (text-width font-size text) 2)
                    half-h (/ font-size 2)]
                [(- x half-w) (- y half-h) (+ x half-w) (+ y half-h)]))
        overlap? (fn [[l1 t1 r1 b1] [l2 t2 r2 b2]]
                   (and (< l1 r2) (< l2 r1) (< t1 b2) (< t2 b1)))]
    (:placed
     (reduce (fn [{:keys [boxes] :as acc} {:keys [offsets forced?] :as label}]
               (let [candidates (for [dy (or offsets [0])]
                                  (update label :y + dy))
                     fits? (fn [l] (not-any? #(overlap? (box l) %) boxes))
                     placed (or (first (filter fits? candidates))
                                (when forced? (first candidates)))]
                 (if placed
                   (-> acc
                       (update :placed conj placed)
                       (update :boxes conj (box placed)))
                   acc)))
             {:placed []
              :boxes (vec obstacles)}
             labels))))

(defn names-layer
  "Country names, from Natural Earth's zoom level for each on, most important
  first, leaving out those that would overlap. Growing a bit as you zoom in
  further. The hovered country's always, also when they're turned off, and
  first. In screen coordinates, to stay upright."
  []
  (let [countries @(rf/subscribe [::countries])
        country-names? @(rf/subscribe [::country-names?])
        hovered-name @hovered
        lon-lat->screen (lon-lat->screen-fn)
        [left top right bottom] (visible-area)
        z (web-zoom)
        labels
        (without-overlaps
         (for [{:keys [name label min-label]}
               (sort-by (juxt #(not= hovered-name (:name %))
                              :min-label
                              (comp - :area))
                        countries)
               :let [hovered? (= hovered-name name)]
               :when (or hovered?
                         (and country-names? (>= z min-label)))
               :let [[x y] (apply lon-lat->screen label)
                     font-size (cond-> (clamp (+ 12 (* 2 (- z min-label))) 11 18)
                                 ;; also when below its zoom level
                                 hovered? (max 13))]
               :when (and (< left x right) (< top y bottom))]
           (cond-> {:x x
                    :y y
                    :font-size font-size
                    :text name}
             ;; Small countries' names would be on their capital: then above
             ;; or below it.
             hovered? (assoc :offsets [0 (- (+ 13 font-size)) (+ 13 font-size)]
                             :forced? true)))
         (capital-boxes (hovered-capitals lon-lat->screen)))]
    (when (or country-names? hovered-name)
      [:g {:fill "#333"
           :stroke "rgba(255, 255, 255, 0.8)"
           :strokeWidth 3
           :strokeLinejoin "round"
           :paintOrder "stroke"
           :fontWeight 600
           :textAnchor "middle"
           :dominantBaseline "middle"
           :pointerEvents "none"
           :style {:userSelect "none"}}
       (for [{:keys [x y font-size text]} labels]
         ^{:key text}
         [:text {:x x
                 :y y
                 :fontSize font-size}
          text])])))

(defn capitals-layer
  "Markers for the hovered country's capitals. In screen coordinates, so
  they're the same size at any zoom."
  []
  ;; not in the for below: it's lazy, so it'd be too late to notice changes
  (let [capitals (hovered-capitals (lon-lat->screen-fn))]
    [:g {:pointerEvents "none"
         :style {:userSelect "none"}}
     (for [{:keys [x y name note]} capitals]
       ^{:key name}
       [capital-marker x y name note])]))

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
     [checkbox "Country names"
      @(rf/subscribe [::country-names?])
      #(rf/dispatch [::set-country-names %])]
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
  (let [{:keys [long-name part-of iso-a2 capitals population population-year]}
        (hovered-country)]
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
        [:div
         [:div {:style {:fontSize "17px"
                        :fontWeight "bold"}}
          long-name]
         (when part-of
           [:div {:style {:color "#777"
                          :marginTop "-2px"}}
            "Part of " part-of])]
        (when iso-a2
          [:div {:style {:fontSize "28px"
                         :lineHeight "1"}}
           (flag-emoji iso-a2)])]
       (let [capital-names (for [{:keys [name note]} capitals]
                             (cond-> name
                               note (str " (" note ")")))]
         (if (next capitals)
           [:div "Capitals:"
            (for [capital capital-names]
              ^{:key capital}
              [:div {:style {:paddingLeft "12px"}} capital])]
           (when (seq capitals)
             [:div "Capital: " (first capital-names)])))
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
           [grid-labels-layer]
           [names-layer]
           [capitals-layer]]
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

;; Settings and the view are saved in the browser's local storage, and restored
;; when the page is loaded.

(def storage-key
  "world/settings")

(defn valid-settings
  "The parts of saved settings that are still valid: they may be from an older
  version, or edited."
  [{:keys [projection resolution south-up? central-meridian country-names? grid
           zoom translate]}]
  (let [finite? #(and (number? %) (js/isFinite %))]
    (cond-> {}
      (contains? proj/projections projection)
      (assoc :projection projection)

      (some #{resolution} (map first resolutions))
      (assoc :resolution resolution)

      (boolean? south-up?)
      (assoc :south-up? south-up?)

      (boolean? country-names?)
      (assoc :country-names? country-names?)

      (and (finite? central-meridian) (<= -180 central-meridian 180))
      (assoc :central-meridian central-meridian)

      (map? grid)
      (assoc :grid (select-keys grid (keys default-grid-settings)))

      (and (finite? zoom) (<= min-zoom zoom max-zoom))
      (assoc :zoom zoom)

      (and (vector? translate) (= 2 (count translate)) (every? finite? translate))
      (assoc :translate translate))))

(defn load-settings []
  (try
    (some-> (.getItem js/localStorage storage-key)
            reader/read-string
            valid-settings)
    ;; storage may be unavailable, or the saved settings unreadable
    (catch :default _
      nil)))

(rf/reg-cofx
  ::saved-settings
  (fn [cofx _]
    (assoc cofx :saved-settings (load-settings))))

(defn save-settings! []
  (let [db @re-frame.db/app-db]
    (try
      (.setItem js/localStorage
                storage-key
                (pr-str (assoc (select-keys db [:projection
                                                :resolution
                                                :south-up?
                                                :central-meridian
                                                :country-names?
                                                :grid])
                               :zoom @zoom
                               :translate @translate)))
      (catch :default _
        nil))))

(defonce save-timeout
  (atom nil))

(defn schedule-save!
  "Saves the settings soon: not on every step while dragging."
  [& _]
  (js/clearTimeout @save-timeout)
  (reset! save-timeout (js/setTimeout save-settings! 300)))

(defn restore-view!
  "Restores the saved zoom and position, instead of fitting the map to the
  view, on page load."
  []
  (when-not @initial-view-set?
    (let [saved (load-settings)]
      (when (and (:zoom saved) (:translate saved))
        (reset! zoom (:zoom saved))
        (reset! translate (:translate saved))
        (reset! initial-view-set? true)))))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (restore-view!)
  (doseq [state [re-frame.db/app-db zoom translate]]
    (add-watch state ::save schedule-save!))
  (r/render (app-root) [app]))
