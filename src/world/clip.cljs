(ns world.clip
  "Cutting countries at the edge of the map, for any central meridian.

  Coordinates are JS arrays of #js [lon lat], as loaded. A map centered on
  longitude c shows the 360° strip [c - 180, c + 180]. A polygon can show up
  partly at one edge and partly at the other: that's two copies of it, one
  shifted by 360°. Each copy is a `strip`: the offset to add to longitudes to
  get map longitudes in [-180, 180], and the visible range in the polygon's
  own longitudes.

  Fills are clipped as polygons. Outlines are clipped as lines, so cutting a
  shape doesn't draw a border along the edge of the map.")

(def antimeridian
  180)

(defn bounds
  "Longitude range [lo hi] of a polygon (its exterior ring)."
  [^js polygon]
  (let [^js ring (aget polygon 0)]
    (loop [i 0
           lo js/Infinity
           hi (- js/Infinity)]
      (if (< i (alength ring))
        (let [x (aget ring i 0)]
          (recur (inc i) (min lo x) (max hi x)))
        [lo hi]))))

(defn strips
  "The copies of a polygon with longitude range [lo hi] that are visible on a
  map centered on longitude `center`, as [offset left right inside?]."
  [[lo hi] center]
  (for [k [-1 0 1]
        :let [offset (- (* 360 k) center)
              left (- -180 offset)
              right (- 180 offset)]
        :when (and (> hi left) (< lo right))]
    [offset left right (and (>= lo left) (<= hi right))]))

(defn- lat-at [^js a ^js b x]
  (let [ax (aget a 0)
        ay (aget a 1)]
    (+ ay (* (- (aget b 1) ay)
             (/ (- x ax) (- (aget b 0) ax))))))

(defn- clip-ring-at
  "Sutherland–Hodgman against the vertical line at x, keeping the side where
  (inside? lon) is true."
  [^js ring x inside?]
  (let [out #js []
        n (alength ring)]
    (dotimes [i n]
      (let [^js a (aget ring (if (zero? i) (dec n) (dec i)))
            ^js b (aget ring i)
            a-in? (inside? (aget a 0))
            b-in? (inside? (aget b 0))]
        (when (not= a-in? b-in?)
          (.push out #js [x (lat-at a b x)]))
        (when b-in?
          (.push out b))))
    out))

(defn- densify-edges
  "Adds points every degree along edges lying on the map's left or right edge,
  so they follow it where it's curved."
  [^js ring left right]
  (let [out #js []
        n (alength ring)
        on-edge? #(or (== % left) (== % right))]
    (dotimes [i n]
      (let [^js a (aget ring i)
            ^js b (aget ring (mod (inc i) n))
            x (aget a 0)
            ay (aget a 1)
            dy (- (aget b 1) ay)
            steps (js/Math.floor (js/Math.abs dy))]
        (.push out a)
        (when (and (on-edge? x) (== x (aget b 0)))
          (loop [j 1]
            (when (< j steps)
              (.push out #js [x (+ ay (* dy (/ j steps)))])
              (recur (inc j)))))))
    out))

(defn- clip-ring [ring left right]
  (let [ring (-> ring
                 (clip-ring-at left #(>= % left))
                 (clip-ring-at right #(<= % right)))]
    (when (>= (alength ring) 3)
      (densify-edges ring left right))))

(defn fill-rings
  "The rings of a polygon copy, clipped to its strip: exterior, then holes."
  [^js polygon [_ left right inside?]]
  (if inside?
    (vec polygon)
    (when-let [exterior (clip-ring (aget polygon 0) left right)]
      (into [exterior]
            (keep #(clip-ring % left right))
            (rest polygon)))))

(defn- artificial-edge?
  "Edges that Natural Earth adds where it cuts shapes at the antimeridian, and
  along the south pole to close Antarctica. They aren't real borders."
  [^js a ^js b]
  (let [ax (aget a 0)]
    (or (and (== antimeridian (js/Math.abs ax))
             (== ax (aget b 0)))
        (and (< (aget a 1) -89.99)
             (< (aget b 1) -89.99)))))

(defn- clip-outline [^js ring left right]
  (let [out #js []
        line (volatile! #js [])
        finish! (fn []
                  (when (>= (alength @line) 2)
                    (.push out @line))
                  (vreset! line #js []))]
    (dotimes [i (dec (alength ring))]
      (let [^js a (aget ring i)
            ^js b (aget ring (inc i))
            ax (aget a 0)
            bx (aget b 0)
            ;; parameter range of the segment within [left, right]
            [t0 t1] (if (== ax bx)
                      (if (<= left ax right) [0 1] [1 0])
                      (let [tl (/ (- left ax) (- bx ax))
                            tr (/ (- right ax) (- bx ax))]
                        [(max 0 (min tl tr)) (min 1 (max tl tr))]))]
        (if (or (>= t0 t1) (artificial-edge? a b))
          (finish!)
          (let [at (fn [t x]
                     (if (== t 0) a (if (== t 1) b #js [x (lat-at a b x)])))
                start (at t0 (if (< ax bx) left right))
                end (at t1 (if (< ax bx) right left))]
            (when (or (pos? t0) (zero? (alength @line)))
              (finish!)
              (.push @line start))
            (.push @line end)
            (when (< t1 1)
              (finish!))))))
    (finish!)
    out))

(defn- any-artificial-edge? [^js ring]
  (loop [i 1]
    (cond
      (>= i (alength ring)) false
      (artificial-edge? (aget ring (dec i)) (aget ring i)) true
      :else (recur (inc i)))))

(defn outline-lines
  "The visible parts of a ring's outline in a strip, as JS arrays of points."
  [^js ring [_ left right inside?]]
  (if (and inside? (not (any-artificial-edge? ring)))
    #js [ring]
    (clip-outline ring left right)))
