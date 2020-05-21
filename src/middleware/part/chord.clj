(ns middleware.part.chord
  (:require [middleware.utils :as util]
            [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

(defn exclude-by-list [coll exclude?]
  (keep-indexed 
    (fn [i item] (if (nth exclude? i) nil item)) coll))

(defn- calculate-colors [n]
  (map (fn [i] (double (* i (/ 360 n)))) (range n)))

(defn filter-out-null-nodes
 "filters out nodes whos sum of the edge-weights are 0
 this includes nodes in a graph that neither have an incoming nor an outgoing edge
 more formally: 
 excludes column and row i if: 
   (forall j in N<(count matrix) matrix[i j] < delta && matrix[j i] < delta)" 
 [matrix nodes]
  (let [delta 0.001
        exclude? (vec 
                   (for [i (range (count matrix))]
                     (every? identity
                       (for [j (range (count matrix))]
                         (and (< (nth (nth matrix i) j) delta)
                              (< (nth (nth matrix j) i) delta))))))
        new-nodes (exclude-by-list nodes exclude?) 
        new-mat (as-> matrix $ 
                  (exclude-by-list $ exclude?)
                  (apply map vector $)
                  (exclude-by-list $ exclude?)
                  (apply map vector $))]
    [new-mat new-nodes]))
(declare chord-diagram)
(defn build [state]
  (let [[mat nodes] (filter-out-null-nodes (:matrix state) (:nodes state))]
    (chord-diagram (assoc state :matrix mat :nodes nodes))))

(defui ChordDiagram
  (render [this {:keys [matrix nodes container-width container-height]}]
    (let 
      [max-radius (/ (min container-width container-height) 2)
       transposed (apply map vector matrix)
       outer-radius (* 0.9 max-radius)
       inner-radius (* outer-radius 0.9)

       xoff (/ (- container-width (* outer-radius 2)) 2)
       yoff (/ (- container-height (* outer-radius 2)) 2)

       n (count matrix)
       spacing (/ (* 2 Math/PI) (* 10 n))
       sub-total (- (* 2 Math/PI) (* n spacing))
       total-value-segments (map #(reduce + %) matrix)
       total-value (reduce + total-value-segments)
       segment-lengths (map #(* sub-total (/ % total-value)) total-value-segments)
       
       segment-radians (util/join-numerical segment-lengths spacing)

       segment-long-path? (map #(if (> % Math/PI) 1 0) segment-lengths)

       chord-data
         (map-indexed 
           (fn [i row]
             (map-indexed 
               (fn [j value]
                 (let [length-from-segment (nth segment-lengths i)
                       length-to-segment (nth segment-lengths j)
                       percentage-from (/ value (nth total-value-segments i)) 
                       to-value (-> matrix (nth j) (nth i))
                       percentage-to (/ to-value (nth total-value-segments j))
                       from-lenght (* length-from-segment percentage-from)
                       to-lenght (* length-to-segment percentage-to)]
                   (if (> j i)
                     {:from-length from-lenght
                      :to-length to-lenght
                      :from-segment i
                      :to-segment j}
                     :skip)))
               row))
           matrix)

       chord-data (map (fn [r] (filter #(not (= :skip %)) r)) chord-data)

       chord-positions 
         (:chord-pos-list
           (reduce
             (fn [acc data]
               (let [phi1-start (acc (:from-segment data))
                     phi1-end (+ phi1-start (:from-length data))
                     phi2-start (acc (:to-segment data))
                     phi2-end (+ phi2-start (:to-length data))
                     from-long-path? (if (> (- phi1-end phi1-start) Math/PI) 1 0)
                     to-long-path? (if (> (- phi2-end phi2-start) Math/PI) 1 0)]
                 (assoc
                   acc
                   (:from-segment data) phi1-end
                   (:to-segment data) phi2-end
                   :chord-pos-list 
                     (if (and
                           (< (Math/abs (- phi1-start phi1-end)) 0.01)
                           (< (Math/abs (- phi2-start phi2-end)) 0.01))
                       (:chord-pos-list acc) 
                       (conj (:chord-pos-list acc) 
                         [(util/to-cartesian phi1-start inner-radius 
                                        [outer-radius outer-radius]) 
                          (util/to-cartesian phi1-end inner-radius 
                                        [outer-radius outer-radius])
                          from-long-path?
                          (util/to-cartesian phi2-start inner-radius 
                                        [outer-radius outer-radius]) 
                          (util/to-cartesian phi2-end inner-radius 
                                        [outer-radius outer-radius])
                          to-long-path?])))))
             (into {} 
               (take n 
                 (map vec 
                   (partition 2 
                     (interleave (range) 
                                 (map first segment-radians))))))
             (flatten chord-data)))

       inner-segment-positions (util/to-cartesian-pairs segment-radians 
                                                   inner-radius [outer-radius 
                                                                 outer-radius])

       outer-segment-positions (util/to-cartesian-pairs segment-radians 
                                                   outer-radius [outer-radius 
                                                                 outer-radius])
       circle-svgs (map
              (fn [[[x1 y1] [x2 y2] [x3 y3] [x4 y4] long-path?]]
                (str "M " (+ xoff x1) " " (+ yoff y1)
                     " A " inner-radius " " inner-radius ", 0, " long-path? ", 1, " 
                           (+ xoff x2) " " (+ yoff y2)
                     " L " (+ xoff x4) " " (+ yoff y4)
                     " A " outer-radius " " outer-radius ", 0, " long-path? ", 0, " 
                           (+ xoff x3) " " (+ yoff y3)
                     " Z"))
              (partition 5
                         (apply 
                           concat 
                           (interleave inner-segment-positions 
                                       outer-segment-positions
                                       (map vector segment-long-path?)))))

       chord-svgs (map
                    (fn [[[x1 y1] [x2 y2] from-long-path? [x3 y3] [x4 y4] to-long-path?]]
                      (str "M " (+ xoff x1) " " (+ yoff y1)
                           " A " inner-radius " " inner-radius ", 0, " from-long-path? ", 1, " 
                                 (+ xoff x2) " " (+ yoff y2)
                           " Q " (+ xoff outer-radius) " " (+ yoff outer-radius) " " 
                                 (+ xoff x3) " " (+ yoff y3)
                           " A " inner-radius " " inner-radius ", 0, " to-long-path? ", 1, " 
                                 (+ xoff x4) " " (+ yoff y4)
                           " Q " (+ xoff outer-radius) " " (+ yoff outer-radius) " " 
                                 (+ xoff x1) " " (+ yoff y1)))
                    chord-positions)
       chord-data-reverse (reverse (flatten chord-data))
       colors (partition 3 (map (fn [_] (int (rand 256))) (range (* 3 n n))))
       chord-colors (calculate-colors (count chord-data-reverse))
       node-colors (calculate-colors (count nodes))]
      (ui/group
        :children (concat
        (map-indexed 
          #(ui/s-v-g-path 
             :id "circle-segment"
             :on-mouse-clicked {:event :clicked-circle-segment
                                :node (nth nodes %1)
                                :node-index %1}
             :content %2
             :style (str "-fx-fill: hsb(" (nth node-colors %1) ",100%,100%);")) 
          circle-svgs)
        (map-indexed 
          #(let [chord-i (nth chord-data-reverse %1)]
             (ui/s-v-g-path 
               :id "chord-segment"
               :on-mouse-clicked {:event :clicked-chord
                                  :edge [(nth nodes (:from-segment chord-i))
                                         (nth nodes (:to-segment chord-i))]
                                  :index %1}
               :content %2

               :style (str "-fx-fill: hsb(" 
                           (nth chord-colors %1) ",100%,100%);"))) 
           chord-svgs))))))


(defui Secondary
  (render [this {:keys [nodes main-font]}]
    (let [colors (calculate-colors (count nodes))] 
      (clojure.pprint/pprint {:nodes nodes :colors colors})
      (ui/grid-pane
        :anchor-pane/top-anchor    0.0
        :anchor-pane/bottom-anchor 0.0
        :anchor-pane/left-anchor   0.0
        :anchor-pane/right-anchor  0.0
        :children 
          (map-indexed
            (fn [idx [node color]]
              (ui/anchor-pane 
                :grid-pane/row-index idx
                :grid-pane/column-index 0
                :grid-pane/row-span 1
                :grid-pane/column-span 1
                :grid-pane/hgrow javafx.scene.layout.Priority/ALWAYS
                :grid-pane/vgrow javafx.scene.layout.Priority/ALWAYS
                :style (str "-fx-background-color: hsb(" color ",100%,100%);")
                :children [(ui/label :font main-font
                                     :text (str node)
                                     )]))
            (partition 2 (interleave nodes colors)))))))
