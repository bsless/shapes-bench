(ns bsless.shapes-bench
  (:require
   [criterium.core :as cc]
   #_[clj-async-profiler.core :as prof])
  (:import
   (java.lang Math)))

(set! *unchecked-math* true)

(println *compiler-options*)

(defmacro bench [body]
  `(let [ret# (cc/quick-benchmark ~body {})
         est# (:mean ret#)
         mean# (first est#)
         [factor# unit#] (cc/scale-time mean#)]
     (cc/report-result ret#)
     (println "\n\n")
     [mean# (cc/format-value mean# factor# unit#)]))

(defn make-circle [n] {:type :circle :radius (rand n)})
(defn make-square [n] {:type :square :side (rand n)})
(defn make-rectangle [n] {:type :rectangle :width (rand n) :height (rand n)})
(defn make-triangle [n] {:type :triangle :width (rand n) :height (rand n)})

(def makers [make-circle make-square make-rectangle make-triangle])

(defn random-shape [n] ((rand-nth makers) n))

(defonce shapes (into [] (repeatedly (int 4e5) #(random-shape 10))))

(defn circle-area [{:keys [radius]}] (* Math/PI radius radius))
(defn square-area [{:keys [side]}] (* side side))
(defn rectangle-area [{:keys [width height]}] (* width height))
(defn triangle-area [{:keys [width height]}] (* 0.5 width height))

(defn shape-area-case [{:keys [type] :as shape}]
  (case type
    :circle (circle-area shape)
    :square (square-area shape)
    :rectangle (rectangle-area shape)
    :triangle (triangle-area shape)))


(defn total-area-case-transduce
  [shapes]
  (transduce (map shape-area-case) + 0 shapes))

(comment)
(println "Case + transduce")
(bench (total-area-case-transduce shapes))
;; => [0.028277429083333333 "28.277429 ms"]

(defn prim-circle-area ^double [shape] (let [radius (unchecked-double (:radius shape))] (* Math/PI radius radius)))
(defn prim-square-area ^double [shape] (let [side (unchecked-double (:side shape))] (* side side)))
(defn prim-rectangle-area ^double [shape] (* ^double (:width shape) ^double (:height shape)))
(defn prim-triangle-area ^double [shape] (* 0.5 ^double (:width shape) ^double (:height shape)))

(defn prim-shape-area-case ^double [shape]
  (case (:type shape)
    :circle (prim-circle-area shape)
    :square (prim-square-area shape)
    :rectangle (prim-rectangle-area shape)
    :triangle (prim-triangle-area shape)))

(defn prim-total-area-case-reduce
  [shapes]
  (reduce (fn ^double [^double acc shape] (+ acc (prim-shape-area-case shape))) 0 shapes))

(defn prim-total-area-case-transduce
  [shapes]
  (transduce (map prim-shape-area-case) + 0 shapes))

(println "Case + prims + transduce")
(bench (prim-total-area-case-transduce shapes))
;; => [0.0168039305 "16.803931 ms"]

(println "Case + prims + reduce")
(bench (prim-total-area-case-reduce shapes))
;; => [0.013422389958333334 "13.422390 ms"]

(defmulti shape-area-multi :type)
(defmethod shape-area-multi :circle [o] (circle-area o))
(defmethod shape-area-multi :square [o] (square-area o))
(defmethod shape-area-multi :rectangle [o] (rectangle-area o))
(defmethod shape-area-multi :triangle [o] (triangle-area o))

(defn total-area-multi-transduce
  [shapes]
  (transduce (map shape-area-multi) + 0 shapes))

(println "multimethod + transduce")
(bench (total-area-multi-transduce shapes))
;; => [0.030941701208333335 "30.941701 ms"]

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

(println "factor + transduce")
(bench (total-area-factor-transduce shapes-factors))
;; => [0.0138719610625 "13.871961 ms"]

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


(println "protocol + transduce")
(bench (total-area-protocol-transduce precord-shapes))
;; => [0.012866039729166667 "12.866040 ms"]
(defrecord Shape [factor width height])

(defonce record-shapes-factors (into [] (map map->Shape) shapes-factors))

(println "shape record + kw access + transduce")
(bench (total-area-factor-transduce record-shapes-factors))
;; => [0.016156389 "16.156389 ms"]
(defn record-shape-area [^Shape shape]
  (* (.factor shape) (.width shape) (.height shape)))

(defn total-area-record-transduce
  [shapes]
  (transduce (map record-shape-area) + 0 shapes))

(println "shape record + member access + transduce")
(bench (total-area-record-transduce record-shapes-factors))
;; => [0.0130048725 "13.004873 ms"]
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

(println "prim shape record + member access + transduce")
(bench (total-area-prim-record record-prim-shapes-factors))
;; => [0.004860275587301587 "4.860276 ms"]

(defn total-area-prim-record-reduce
  [shapes]
  (reduce (fn ^double [^double acc shape] (+ acc (prim-record-shape-area shape))) 0 shapes))

(println "prim shape record + member access + reduce")
(bench (total-area-prim-record-reduce record-prim-shapes-factors))
;; => [0.002313144886524823 "2.313145 ms"]

(defn get-area ^double [^IPrimShape shape]
  (.getArea shape))

(defn total-area-prim-record-method [shapes]
  (transduce (map get-area) + 0 shapes))

(println "prim shape record + interface call + transduce")
(bench (total-area-prim-record-method record-prim-shapes-factors))
;; => [0.004738504543478262 "4.738505 ms"]

(defn total-area-prim-record-method-reduce
  [shapes]
  (reduce (fn ^double [^double acc shape] (+ acc (get-area shape))) 0 shapes))

(println "prim shape record + interface call + reduce")
(bench (total-area-prim-record-method-reduce record-prim-shapes-factors))
;; => [0.002061013670068027 "2.061014 ms"]

(import java.util.Iterator)

(defn total-area-prim-record-loop
  ^double [^Iterable shapes]
  (let [^Iterator it (.iterator shapes)]
    (loop [sum 0.0]
      (if (.hasNext it)
        (recur (unchecked-add sum (prim-record-shape-area (.next it))))
        sum))))

(println "prim shape record + loop")
(bench (total-area-prim-record-loop record-prim-shapes-factors))
;; => [0.0011672441529411767 "1.167244 ms"]

(set! *warn-on-reflection* true)

(def df (reify java.util.function.ToDoubleFunction
          (applyAsDouble [_ shape]
            (prim-record-shape-area shape))))

(defn total-area-stream ^double [shapes]
  (.sum (.mapToDouble (.stream ^java.util.Collection shapes) df)))

(println "prim shape record + stream")
(bench (total-area-stream record-prim-shapes-factors))
;; => [0.003212101953125 "3.212102 ms"]
#_
(prof/serve-files 7777)
#_
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

(println "prim shape record + array loop")
(bench (total-area-prim-record-loop-array record-prim-shapes-factors-array))
;; => [7.437256957070708E-4 "743.725696 µs"]
#_
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

(println "prim shape type + array loop")
(bench (total-area-prim-type-loop-array type-prim-shapes-factors-arr))
;; => [4.1439404928664074E-4 "414.394049 µs"]
