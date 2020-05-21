(ns middleware.part.buttonbar
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

(defui ButtonBar
  (render [this state]
    (ui/flow-pane
      :children (map #(ui/button 
                        :font (:main-font state) 
                        :style-class ["rectbutton"]
                        :text (clojure.string/replace (name %) #"-" " ")
                        :on-action {:event :switch-current-view
                                    :to %}) 
                     (:view-options state)))))
