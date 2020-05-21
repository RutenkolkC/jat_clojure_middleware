(ns middleware.part.graphlisting 
  (:require [middleware.utils :as utils]
            [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

(defui PackageView
  (render [this {:keys [zoom-level main-fx-font main-font
                        packageview-translate-x packageview-translate-y]
                 {:keys [nodes edges]} :current-package-view}]
    (if (empty? nodes) 
      (ui/label :font main-font :text "Nothing found")
      (let [node-offset 50
          y-padding 5
          nodes-dims (map #(utils/string-dims main-fx-font % [0 5]) nodes)
          max-width (apply max (map :width nodes-dims))
          node-offsets (reduce (fn [acc v] (concat acc [(+ y-padding (last acc) (:height v))])) [0] nodes-dims)
          nodes-indexed (into {} (map-indexed (comp vec reverse vector) nodes))

          max-left-distance-ind (apply max (cons 0 (filter #(<= 0 %) (map (fn [[f t]] (- (nodes-indexed t) (nodes-indexed f))) edges))))
          max-right-distance-ind (apply max (cons 0 (filter #(> 0 %) (map (fn [[f t]] (- (nodes-indexed f) (nodes-indexed t))) edges))))
          max-left-distance 0
          ]
      (ui/group 
        :scale-x zoom-level
        :scale-y zoom-level
        :translate-y (+ packageview-translate-y
                      (- (first (rest node-offsets)) (first node-offsets)))
        :translate-x (+ packageview-translate-x
                      (+ 10 (* 0.5 max-left-distance-ind (- (first (rest node-offsets)) (first node-offsets)))))
        :on-scroll {:event :package-view-scroll
                    :fn-fx/include {:fn-fx/event #{:delta-y}}}
        :children (concat 
          (map (fn [[from to]] 
            (let [from-i (nodes-indexed from)
                  to-i (nodes-indexed to)
                  y1 (nth node-offsets from-i)
                  y2 (nth node-offsets to-i)
                  [y1 y2] (if (= from to)
                            [(- y1 1) (+ y2 1)]
                            [y1 y2])
                  x1 max-left-distance
                  forward-edge? (<= (- from-i to-i) 0)
                  x1_off (if forward-edge? 0 max-width)
                  x2 max-left-distance
                  x2_off (if forward-edge? 0 max-width)
                  radius (if (= from to) 
                           (/ (:height (nth nodes-dims from-i)) 3)
                           (double (/ (- y1 y2) 2)))
                  color (if forward-edge? "rgb(0,255,0)" "rgb(255,0,0)")
                  svg-str (str "M " (+ x1 x1_off) " " y1 
                               " A " radius " " radius " 0 1 0 " (+ x2 x2_off) " " y2)]
              (ui/s-v-g-path 
                :content svg-str
                :on-mouse-clicked {:event :clicked-arc
                                   :edge-selected [from to]}
                :style-class (if forward-edge? 
                               ["package-view-forward-edge"]
                               ["package-view-backwards-edge"]))
              
              )) edges)
           (map-indexed 
             (fn [i n]
               (ui/rectangle 
                 :x max-left-distance 
                 :y (- (nth node-offsets i) (/ (:height (nth nodes-dims i)) 2))
                 :width max-width
                 :height (:height (nth nodes-dims i))
                 :arc-width 20
                 :arc-height 20
                 :on-mouse-clicked {:event :class-clicked 
                                    :class n}
                 :style "-fx-fill: rgb(0,172,237);"
                 )) nodes)
           (map-indexed 
             (fn [i n] 
               (ui/text 
                 :text n
                 :font main-font
                 :y (+ (nth node-offsets i) (/ (:height (nth nodes-dims i)) 4))
                 :x (+ max-left-distance 
                       (/ (- max-width (:width (nth nodes-dims i))) 2))
                 :on-mouse-clicked {:event :class-clicked 
                                    :class n})) nodes)))))))
