(ns middleware.utils)

(defmacro fit-to-parent [node]
  (concat 
    node
    [:anchor-pane/top-anchor    0.0
     :anchor-pane/bottom-anchor 0.0
     :anchor-pane/left-anchor   0.0
     :anchor-pane/right-anchor  0.0]))
(defmacro fit-to-parent-anchor [& args]
  (let [prop-vals (drop-last args)
        node (last args)] 
   (concat
     `(ui/anchor-pane
        :anchor-pane/top-anchor    0.0
        :anchor-pane/bottom-anchor 0.0
        :anchor-pane/left-anchor   0.0
        :anchor-pane/right-anchor  0.0  
        :children)
     [node]
     prop-vals)))
;switching to java-11 from java-8 was a surprise as my beloved FontMetrics are no longer available :(
(defn string-dims 
  ([font string]
   (let [text-node (javafx.scene.text.Text.)
         _ (.setFont text-node font)
         _ (.setText text-node string)]
     {:width (-> text-node .getLayoutBounds .getWidth)
      :height (-> text-node .getLayoutBounds .getHeight)}))
  ([font string [x-padding y-padding]]
   (let [text-node (javafx.scene.text.Text.)
         _ (.setFont text-node font)
         _ (.setText text-node string)] 
     {:width (+ x-padding x-padding (-> text-node .getLayoutBounds .getWidth))
      :height (+ y-padding y-padding (-> text-node .getLayoutBounds .getHeight))})))

(defn join-numerical [in spacing] 
  (reduce 
    (fn [acc len] 
      (conj 
        acc
        [(+ (second (last acc)) spacing) 
         (+ (second (last acc)) len spacing)])) 
    [[0 (first in)]]
    (rest in)))
(defn to-cartesian 
  ([phi radius] 
   [(* radius (Math/cos phi)) 
    (* radius (Math/sin phi))])
  ([phi radius [offx offy]] 
   [(+ offx (* radius (Math/cos phi))) 
    (+ offy (* radius (Math/sin phi)))]))

(defn to-cartesian-pairs 
  ([pairs radius] 
   (map (fn [[phi1 phi2]] 
          [[(* radius (Math/cos phi1)) 
            (* radius (Math/sin phi1))] 
           [(* radius (Math/cos phi2)) 
            (* radius (Math/sin phi2))]]) 
        pairs))
  ([pairs radius [offx offy]] 
   (map (fn [[phi1 phi2]] 
          [[(+ offx (* radius (Math/cos phi1))) 
            (+ offy (* radius (Math/sin phi1)))] 
           [(+ offx (* radius (Math/cos phi2))) 
            (+ offy (* radius (Math/sin phi2)))]]) 
        pairs)))
