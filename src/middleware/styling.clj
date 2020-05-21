(ns middleware.styling
  (:require [garden.core :as garden]))

(defn generate-css [] 
  (let [series-colors ["#00A0B0" "#6A4A3C" "#CC333F" "#EB6841" "#EDC951"]]
    (garden/css {:output-to "resources/main.css"} 
        [:* {:primary     "#2A2E37"
             :secondary   "#FFFF8D"
             :primarytext "#B2B2B2"
             :blue        "#1976D2"
             :red         "#FF0000"
             :color-1     "#1976D2"}]
        [:.root * {:-fx {:background-color :primary}}]

        [:.text-field {:-fx {:background-color "derive(primary, 20%)"
                             :text-fill "derive(secondary, 20%)"}}
         [:&:hover {:fx {:text-fill :secondary}}]
         [:&:focused {:fx {:text-fill :secondary}}] ]
        [:.label {:-fx {:background-color :primary
                             :text-fill :primarytext}}
         [:&:hover {:fx {:text-fill :secondary}}]
         [:&:focused {:fx {:text-fill :secondary}}] ]
        [:.package-view-forward-edge
         {:-fx {:fill "none"
                :stroke-width 3
                :stroke :green}}
         [:&:hover {:fx {:opacity 1 
                         :stroke-width 5
                         :scale-x 1.05
                         :scale-y 1.05}}]]
        [:.package-view-backwards-edge
         {:-fx {:fill "none"
                :stroke-width 3
                :stroke :red}}
         [:&:hover {:fx {:opacity 1 
                         :stroke-width 5
                         :scale-x 1.05
                         :scale-y 1.05}}]]

        [:.main-view-container
         {:-fx { :background-color :primary
                 :border-color :secondary
                 :border-radius 2}} ]

        [:.button1
         {:-fx
          {:text-fill :primarytext
           :background-color :primary
           :border-color :secondary
           :border-radius 0 
           :background-radius 0
           :pref-height 400
           }}
         [:&:hover
           {:-fx {:background-color :secondary
                  :text-fill :primary}}]]
        [:.rectbutton
         {:-fx
          {:text-fill :primarytext
           :background-color :primary
           :border-color :secondary
           :border-radius 0 
           :background-radius 0}}
         [:&:hover
           {:-fx {:background-color :secondary
                  :text-fill :primary}}]]
        ["#circle-segment"
         {:-fx {:fill "rgb(128,128,128)"
                :stroke-width 2
                :stroke "rgb(0,0,0)"
                :opacity 0.6}}

         [:&:hover {:-fx {:opacity 1
                          :fill :white
                          :scale-x 1.1
                          :scale-y 1.1}}]]
      
        ["#chord-segment"
         {:-fx {:fill "rgb(128,128,128)"
                :stroke-width 1
                :stroke "rgb(255,255,255)"
                :opacity 0.5}}

         [:&:hover {:-fx {:opacity 1
                          :fill :white
                          :scale-x 1.1
                          :scale-y 1.1}}]]
        )))
