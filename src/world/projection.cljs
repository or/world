(ns world.projection
  "Map projections: (fn [lon lat width height]) -> [x y], with y pointing
  down, lon 0 / lat 0 at x = width / 2, y = 0.")

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

(defn lat-at-y
  "The latitude at map y, found by bisection."
  [proj-fn y width height]
  (loop [lo -90
         hi 90
         i 0]
    (let [mid (/ (+ lo hi) 2)]
      (cond
        (= i 50) mid
        (> (second (proj-fn 0 mid width height)) y) (recur mid hi (inc i))
        :else (recur lo mid (inc i))))))

(defn max-lat
  "Beyond this latitude, the projection clamps."
  [projection]
  (case projection
    :mercator 85
    :miller 89.5
    90))

(defn unproject
  "Inverse of proj-fn: map coordinates -> [lon lat]. Found numerically, which
  works for all projections here, because y depends only on latitude (and
  decreases as it grows), and for a fixed latitude x is linear in longitude."
  [proj-fn x y width height]
  (let [lat (lat-at-y proj-fn y width height)
        [x0] (proj-fn 0 lat width height)
        [x180] (proj-fn 180 lat width height)
        lon (if (== x0 x180)
              0
              (* 180 (/ (- x x0) (- x180 x0))))]
    [(-> lon (max -180) (min 180)) lat]))
