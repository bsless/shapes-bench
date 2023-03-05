(ns bsless.shapes-bench
  (:require
   [criterium.core :as cc]
   [clj-async-profiler.core :as prof])
  (:import
   (java.lang Math)))

(set! *unchecked-math* true)

(defmacro bench [body]
  `(let [ret# (cc/quick-benchmark ~body {})
         est# (:mean ret#)
         mean# (first est#)
         [factor# unit#] (cc/scale-time mean#)]
     (cc/report-result ret#)
     [mean# (cc/format-value mean# factor# unit#)]))

(defn make-circle [n] {:type :circle :radius (rand n)})
(defn make-square [n] {:type :square :side (rand n)})
(defn make-rectangle [n] {:type :rectangle :width (rand n) :height (rand n)})
(defn make-triangle [n] {:type :triangle :width (rand n) :height (rand n)})

(def makers [make-circle make-square make-rectangle make-triangle])

(defn random-shape [n] ((rand-nth makers) n))

(defonce shapes (into [] (repeatedly (int 1e5) #(random-shape 10))))

(defn circle-area [{:keys [radius]}] (* Math/PI radius radius))
(defn square-area [{:keys [side]}] (* side side))
(defn rectangle-area [{:keys [width height]}] (* width height))
(defn triangle-area [{:keys [width height]}] (* 0.5 width height))

(defn shape-area-case [{:keys [type] :as shape}]
  ((case type
     :circle circle-area
     :square square-area
     :rectangle rectangle-area
     :triangle triangle-area)
   shape))


(defn total-area-case-transduce
  [shapes]
  (transduce (map shape-area-case) + 0 shapes))

(comment
  (bench (total-area-case-transduce shapes))
  ;; => [0.007121097388888889 "7.121097 ms"]
  ;; => [0.006868884777777778 "6.868885 ms"]
  ;; => [0.006944577633333333 "6.944578 ms"]
  )

(defmulti shape-area-multi :type)
(defmethod shape-area-multi :circle [o] (circle-area o))
(defmethod shape-area-multi :square [o] (square-area o))
(defmethod shape-area-multi :rectangle [o] (rectangle-area o))
(defmethod shape-area-multi :triangle [o] (triangle-area o))

(defn total-area-multi-transduce
  [shapes]
  (transduce (map shape-area-multi) + 0 shapes))

(total-area-multi-transduce shapes)
;; => 4388931.373489026
(comment
  (bench (total-area-multi-transduce shapes))
  ;; => [0.006816581177777778 "6.816581 ms"]
  )

(defonce shapes-factors
  (into
   []
   (map (fn [{:keys [type] :as shape}]
          (case type
            :circle {:factor Math/PI :width (:radius shape) :height (:radius shape)}
            :square {:factor 1.0 :width (:side shape) :height (:side shape)}
            :rectangle {:factor 1.0 :width (:width shape) :height (:height shape)}
            :triangle {:factor 0.5 :width (:width shape) :height (:height shape)}
            )))
   shapes))

(defn shape-area-factor
  [shape]
  (* (:factor shape) (:width shape) (:height shape)))

(defn total-area-factor-transduce
  [shapes]
  (transduce (map shape-area-factor) + 0 shapes))

(total-area-factor-transduce shapes-factors)

(comment
  (bench (total-area-factor-transduce shapes-factors))
  ;; => [0.003435118416666667 "3.435118 ms"]
  )

(defprotocol PArea
  (-area [shape]))

(defrecord Circle [radius] PArea (-area [_] (* Math/PI radius radius)))
(defrecord Square [side] PArea (-area [_] (* side side)))
(defrecord Rectangle [width height] PArea (-area [_] (* width height)))
(defrecord Triangle [width height] PArea (-area [_] (* 0.5 width height)))

(defonce precord-shapes
  (into
   []
   (map (fn [{:keys [type] :as shape}]
          (case type
            :circle (->Circle (:radius shape))
            :square (->Square (:side shape))
            :rectangle (->Rectangle (:width shape) (:height shape))
            :triangle (->Triangle (:width shape)(:height shape))
            )))
   shapes))

(defn total-area-protocol-transduce
  [shapes]
  (transduce (map -area) + 0 shapes))


(comment (bench (total-area-protocol-transduce precord-shapes)))
;; => [0.0033820822258064523 "3.382082 ms"]

(defrecord Shape [factor width height])

(defonce record-shapes-factors (into [] (map map->Shape) shapes-factors))

(bench (total-area-factor-transduce record-shapes-factors))
;; => [0.0025976192083333335 "2.597619 ms"]

(defn record-shape-area [^Shape shape]
  (* (.factor shape) (.width shape) (.height shape)))

(defn total-area-record-transduce
  [shapes]
  (transduce (map record-shape-area) + 0 shapes))

(bench (total-area-record-transduce record-shapes-factors))
;; => [0.0021780838537414967 "2.178084 ms"];; => [0.002310101343537415 "2.310101 ms"]

(definterface IPrimShape
  (^double getArea []))

(defrecord PrimShape [^double factor ^double width ^double height]
  IPrimShape
  (getArea [_]
    (* factor width height)))

(def record-prim-shapes-factors (into [] (map map->PrimShape) shapes-factors))

(defn prim-record-shape-area ^double [^PrimShape shape]
  (* (.factor shape) (.width shape) (.height shape)))

(defn total-area-prim-record
  [shapes]
  (transduce (map prim-record-shape-area) + 0 shapes))

(bench (total-area-prim-record record-prim-shapes-factors))
;; => [0.0014644022083333333 "1.464402 ms"]

(defn get-area ^double [^IPrimShape shape]
  (.getArea shape))

(defn total-area-prim-record-method [shapes]
  (transduce (map get-area) + 0 shapes))

(bench (total-area-prim-record-method record-prim-shapes-factors))
;; => [0.001462911894607843 "1.462912 ms"]

(import java.util.Iterator)

(defn total-area-prim-record-loop
  ^double [^Iterable shapes]
  (let [^Iterator it (.iterator shapes)]
    (loop [sum 0.0]
      (if (.hasNext it)
        (recur (unchecked-add sum (prim-record-shape-area (.next it))))
        sum))))

(bench (total-area-prim-record-loop record-prim-shapes-factors))
;; => [1.7373949457762558E-4 "173.739495 µs"]

(set! *warn-on-reflection* true)

(def df (reify java.util.function.ToDoubleFunction
          (applyAsDouble [_ shape]
            (prim-record-shape-area shape))))

(defn total-area-stream ^double [shapes]
  (.sum (.mapToDouble (.stream ^java.util.Collection shapes) df)))

(bench (total-area-stream record-prim-shapes-factors))
;; => [7.799451511627907E-4 "779.945151 µs"]

(prof/serve-files 7777)
(time
 (prof/profile
  (dotimes [_ 1e5]
    (total-area-prim-record-loop record-prim-shapes-factors))))

(def record-prim-shapes-factors-array
  (into-array PrimShape record-prim-shapes-factors))

(defn total-area-prim-record-loop-array
  ^double [^objects shapes]
  (let [n (long (alength shapes))]
    (loop [sum 0.0
           i 0]
      (if (= i n)
        sum
        (recur (unchecked-add sum (prim-record-shape-area (aget ^objects shapes i)))
               (unchecked-inc i))))))

(bench (total-area-prim-record-loop-array record-prim-shapes-factors-array))
;; => [9.696065559833175E-5 "96.960656 µs"]

(time
 (prof/profile
  (dotimes [_ 1e5]
    (total-area-prim-record-loop-array record-prim-shapes-factors-array))))

(deftype PrimShapeType [^double factor ^double width ^double height])

(defn prim-record-shape-type-area ^double [^PrimShapeType shape]
  (* (.factor shape) (.width shape) (.height shape)))

(defn total-area-prim-type-loop-array
  ^double [^objects shapes]
  (let [n (long (alength shapes))]
    (loop [sum 0.0
           i 0]
      (if (= i n)
        sum
        (recur (unchecked-add sum (prim-record-shape-type-area (aget ^objects shapes i)))
               (unchecked-inc i))))))

(def type-prim-shapes-factors
  (into []
        (map (fn [shape] (new PrimShapeType (:factor shape) (:width shape) (:height shape))))
        shapes-factors))

(def type-prim-shapes-factors-arr (into-array PrimShapeType type-prim-shapes-factors))

(bench (total-area-prim-type-loop-array type-prim-shapes-factors-arr))
;; => [8.878409546783626E-5 "88.784095 µs"]
