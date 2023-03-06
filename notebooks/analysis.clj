(ns analysis
  (:require
   [nextjournal.clerk :as clerk]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(comment
  (clerk/serve! {:port 7777 :browse? true :watch-paths ["src" "notebooks"]}))

(defn read-results
  [f]
  (with-open [r (java.io.PushbackReader. (io/reader (io/file f)))]
    (edn/read {:default (fn [_ v] v)} r)))

(def results
  [["graal19" (read-results  "./results/results-19.0.2+7-jvmci-22.3-b12.edn")]
   ["jdk19" (read-results "./results/results-19.0.2+7-Ubuntu-0ubuntu322.04.edn")]])

(clerk/vl
 {:mark "bar"
  :encoding {:x {:field "category"
                 :sort {:field "mean"}}
             :y {:field "mean" :type "quantitative" :scale {:type "log"}}
             :color {:field "group"}
             :xOffset {:field "group"}}
  :width 800
  :height 600
  :data {:values (for [[group results] results
                       [k m] results
                       :let [k (-> k
                                   (str/replace "shape-" "")
                                   (str/replace "-" " "))]]
                   {:category k #_(str/split k #"-")
                    :group group
                    :mean (first (:mean m))})}})

(clerk/vl
 {:mark "bar"
  :encoding {:x {:field "category"
                 ;; :type "ordinal"
                 :sort {:field "mean"}
                 ;; :labelPadding 10
                 ;; :bandPaddingInner 10
                 ;; :barBandPaddingInner 10
                 ;; :padding 50
                 ;; :labelAngle 45
                 }
             :y {:field "mean" :type "quantitative" :scale {:type "log"}}
             :color {:field "group"}
             :xOffset {:field "group"}}
  :width 800
  :height 600
  :data {:values (for [[group results] results
                       [k m] results]
                   {:category k #_(str/split k #"-")
                    :group group
                    :mean (first (:mean m))})}})


(clerk/vl
 {:mark "bar"
  :encoding {:x {:field "category"
                 :sort {:field "value"}}
             :y {:field "value" :type "quantitative"}}
  :width 800
  :height 600
  :data {:values (let [[[_ graal] [_ jdk]] results]
                   (for [[k m] graal
                         :let [jdkm (get jdk k)]]
                     {:category k
                      :value (- 1
                                (/ (first (:mean m)) (first (:mean jdkm))))}))}})
