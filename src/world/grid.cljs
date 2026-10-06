(ns world.grid
  "Latitude/longitude grid lines and their labels."
  (:require
   [world.projection :as proj]))

(def steps
  [1 2 5 10 15 30])

(defn auto-step
  "Grid spacing for a zoom level: at most about 12 meridians across the view."
  [zoom]
  (or (first (filter #(<= (/ 360 zoom %) 12) steps))
      (last steps)))

(defn- multiples
  "Multiples of step in [lo, hi]."
  [step lo hi]
  (range (* step (js/Math.ceil (/ lo step)))
         (+ (* step (js/Math.floor (/ hi step))) (/ step 2))
         step))

(defn latitudes [step max-lat]
  (remove #(>= (js/Math.abs %) 90)
          (multiples step (- max-lat) max-lat)))

(defn longitudes
  "Grid longitudes visible on a map centered on `center`, which may lie
  outside [-180, 180]."
  [step center]
  (multiples step (- center 180) (+ center 180)))

(defn normalize-lon [lon]
  (- (mod (+ lon 180) 360) 180))

(defn format-longitude [lon]
  (let [lon (normalize-lon lon)]
    (cond
      (zero? lon) "0°"
      (== -180 lon) "180°"
      (pos? lon) (str lon "°E")
      :else (str (- lon) "°W"))))

(defn format-latitude [lat]
  (cond
    (zero? lat) "0°"
    (pos? lat) (str lat "°N")
    :else (str (- lat) "°S")))

(defn- line->path [points]
  (let [out #js []]
    (doseq [[i [x y]] (map-indexed vector points)]
      (.push out
             (if (zero? i) "M" "L")
             (/ (js/Math.round (* x 100)) 100)
             ","
             (/ (js/Math.round (* y 100)) 100)))
    (.join out "")))

(defn- parallel [proj-fn height lat]
  (for [lon (range -180 181)]
    (proj-fn lon lat 1000 height)))

(defn- meridian [proj-fn height max-lat lon]
  (for [lat (concat (range (- max-lat) max-lat) [max-lat])]
    (proj-fn lon lat 1000 height)))

(defn grid-path
  "All grid lines, in map coordinates, as an SVG path."
  [projection center step]
  (let [[proj-fn height] (proj/get-projection projection)
        max-lat (proj/max-lat projection)]
    (apply str
           (concat
            (for [lon (longitudes step center)]
              (line->path (meridian proj-fn height max-lat (- lon center))))
            (for [lat (latitudes step max-lat)]
              (line->path (parallel proj-fn height lat)))))))

(defn equator-path [projection]
  (let [[proj-fn height] (proj/get-projection projection)]
    (line->path (parallel proj-fn height 0))))

(defn- clamp [v a b]
  (-> v (max a) (min b)))

(def font-size
  12)

(def padding
  4)

(defn- inside-map? [proj-fn height [x y]]
  (let [lat (proj/lat-at-y proj-fn y 1000 height)
        [x0] (proj-fn 0 lat 1000 height)
        [x180] (proj-fn 180 lat 1000 height)]
    (and (< (js/Math.abs lat) 90)
         (<= (js/Math.abs (- x x0)) (js/Math.abs (- x180 x0))))))

(defn prime-meridians
  "Where the prime meridian is on a map centered on `center`: once, or on
  both edges when the center is ±180°."
  [center]
  (longitudes 360 center))

(defn prime-meridian-path [projection center]
  (let [[proj-fn height] (proj/get-projection projection)
        max-lat (proj/max-lat projection)]
    (apply str
           (for [lon (prime-meridians center)]
             (line->path (meridian proj-fn height max-lat (- lon center)))))))

(defn labels
  "Grid labels in screen (viewBox) coordinates. Longitudes go along the
  equator, latitudes along the prime meridian. Either is moved to the edge of
  the view when its line is out of view.

  to-screen and from-screen convert between map and screen coordinates;
  visible is the visible area in screen coordinates: [left top right bottom]."
  [{:keys [projection center step grid? equator? prime-meridian? to-screen
           from-screen visible]}]
  (let [[proj-fn height] (proj/get-projection projection)
        max-lat (proj/max-lat projection)
        [left top right bottom] visible
        ;; The center of the map: on the equator and the central meridian.
        [cx cy] (to-screen [500 0])
        lon-y (clamp (+ cy padding)
                     (+ top padding)
                     (- bottom padding font-size))
        lon-lat (proj/lat-at-y proj-fn (second (from-screen [cx lon-y])) 1000 height)
        ;; The prime meridian, relative to the central one. When the map is
        ;; centered on 180°, it's on both edges: this is the left one.
        prime-lon (normalize-lon (- center))
        lon-labels? (and (or grid? prime-meridian?)
                         (< (js/Math.abs lon-lat) max-lat))]
    (concat
     (when lon-labels?
       (for [lon (cond
                   grid? (longitudes step center)
                   prime-meridian? (prime-meridians center))
             :let [[x] (to-screen (proj-fn (- lon center) lon-lat 1000 height))]
             ;; roughly half a label's width from the edges
             :when (<= (+ left (* 2 font-size)) x (- right (* 2 font-size)))]
         {:key (str "lon" lon)
          :x x
          :y lon-y
          :text (if (and prime-meridian? (zero? (normalize-lon lon)))
                  "Prime meridian"
                  (format-longitude lon))
          :anchor "middle"
          :baseline "hanging"}))
     (for [lat (cond-> (if grid? (latitudes step max-lat) [])
                 (and equator? (not grid?)) (conj 0))
           ;; The prime meridian may be curved: where it crosses this
           ;; latitude.
           :let [[x y] (to-screen (proj-fn prime-lon lat 1000 height))
                 lat-x (clamp (+ x padding)
                              (+ left padding)
                              (- right padding (* 3.5 font-size)))]
           :when (and (<= (+ top padding font-size) y (- bottom padding))
                      ;; not on the row of longitude labels: they're below
                      ;; lon-y, these above y
                      (not (and lon-labels?
                                (< (- lon-y padding)
                                   y
                                   (+ lon-y font-size padding font-size))))
                      ;; the label's start and end are on the map
                      (inside-map? proj-fn height (from-screen [lat-x y]))
                      (inside-map? proj-fn height
                                   (from-screen [(+ lat-x (* 3 font-size)) y])))]
       {:key (str "lat" lat)
        :x lat-x
        :y (- y 3)
        :text (if (and equator? (zero? lat))
                "Equator"
                (format-latitude lat))
        :anchor "start"
        :baseline "auto"}))))
