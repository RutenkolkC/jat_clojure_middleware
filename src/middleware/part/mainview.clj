(ns middleware.part.mainview
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]
            [middleware.part.graphlisting :as graphlisting]
            [middleware.part.chord :as chord]))

(defui SearchBar
  (render [this state]
    (ui/text-field 
      :id :search-bar
      :prompt-text "please enter packagename"
      :font (:main-font state)
      :on-action {:event :search-for
                  :fn-fx/include {:search-bar #{:text}}})))

(defui MainView
  (render [this state]
    (ui/grid-pane 
      :v-box/vgrow javafx.scene.layout.Priority/ALWAYS
      :column-constraints [(ui/column-constraints :percent-width 25)
                           (ui/column-constraints :percent-width 50)
                           (ui/column-constraints :percent-width 25)]
      :children [(ui/anchor-pane
                   :style-class ["main-view-container"]
                   :grid-pane/row-index 0
                   :grid-pane/column-index 0
                   :grid-pane/row-span 1
                   :grid-pane/column-span 1
                   :grid-pane/hgrow javafx.scene.layout.Priority/ALWAYS
                   :grid-pane/vgrow javafx.scene.layout.Priority/ALWAYS
                   :children 
                    [(ui/v-box
                     :anchor-pane/top-anchor    0.0
                     :anchor-pane/bottom-anchor 0.0
                     :anchor-pane/left-anchor   0.0
                     :anchor-pane/right-anchor  0.0
                     :children [(search-bar state)]
                     )])
                 (ui/anchor-pane
                   :style-class ["main-view-container"]
                   :grid-pane/row-index 0
                   :grid-pane/column-index 1
                   :grid-pane/row-span 1
                   :grid-pane/column-span 1
                   :grid-pane/hgrow javafx.scene.layout.Priority/ALWAYS
                   :grid-pane/vgrow javafx.scene.layout.Priority/ALWAYS
                   :listen/height {:event :main-container-height-change}
                   :listen/width {:event :main-container-width-change }
                   :clip (ui/rectangle :width (-> state :main-view :main-container :width)
                                       :height (-> state :main-view :main-container :height))
                   :children [
                     (cond
                       (= (:current-view state) :chord-diagram-view)
                         (chord/build {:matrix (-> state :chord-diagram :matrix)
                                       :nodes (-> state :chord-diagram :nodes)
                                       :container-width (-> state :main-view :main-container :width)
                                       :container-height (-> state :main-view :main-container :height)})
                       (= (:current-view state) :package-viewer)
                         (graphlisting/package-view state)
                       :default 
                         (ui/label :text "nothing here :)" :font (:main-font state)))])
                 (ui/anchor-pane 
                   :style-class ["main-view-container"]
                   :grid-pane/row-index 0
                   :grid-pane/column-index 2
                   :grid-pane/row-span 1
                   :grid-pane/column-span 1
                   :grid-pane/hgrow javafx.scene.layout.Priority/ALWAYS
                   :grid-pane/vgrow javafx.scene.layout.Priority/ALWAYS
                   :children [
                     (cond
                       (= (:current-view state) :chord-diagram-view)
                         (chord/secondary {:nodes (-> state :chord-diagram :nodes)
                                           :main-font (-> state :main-font)})
                       :default 
                         (ui/label 
                           :anchor-pane/top-anchor    0.0
                           :anchor-pane/bottom-anchor 0.0
                           :anchor-pane/left-anchor   0.0
                           :anchor-pane/right-anchor  0.0
                           :font (:main-font state) 
                           :text "No secondary view available"))])])))
