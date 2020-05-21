(ns middleware.part.menubar
  (:require [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

(defui MenuBar
  (render [this state]
    (ui/menu-bar
      :menus [(ui/menu 
                :text "File"
                :items [(ui/menu-item :text "aaaa")
                        (ui/menu-item :text "aaaa")])
              (ui/menu 
                :text "Edit"
                :items [(ui/menu-item :text "aaaa")
                        (ui/menu-item :text "aaaa")])
              (ui/menu 
                :text "View"
                :items (map #(ui/menu-item 
                                :text (clojure.string/replace (name %) #"-" " ")
                                :on-action {:event :switch-current-view
                                            :to %}) 
                             (:view-options state)))
              (ui/menu 
                :text "help"
                :items [(ui/menu-item :text "About")
                        (ui/menu-item :text "aaaa")])
              ])))
