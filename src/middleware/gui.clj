(ns middleware.gui
  (:require [middleware.core :as jat]
            [middleware.part.chord :as chord]
            [middleware.part.menubar :as menubar]
            [middleware.part.mainview :as mainview]
            [middleware.part.buttonbar :as buttonbar]
            [middleware.part.graphlisting :as graphlisting]
            [middleware.styling :as styling]
            [middleware.utils :as utils]
            [fn-fx.fx-dom :as dom]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.controls :as ui]))

(defui Sidebar
  (render [this {:keys [menu-options current-menu sidebar-visible? main-font]}]
    (ui/v-box
      :h-box/hgrow javafx.scene.layout.Priority/ALWAYS
      :fill-width true
      :children
        (concat
          [(ui/button
             :font main-font
             :v-box/vgrow javafx.scene.layout.Priority/ALWAYS
             :style 
             "-fx-background-image: url('https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Hamburger_icon.svg/1024px-Hamburger_icon.svg.png');
              -fx-background-size: 50px;
              -fx-background-repeat: no-repeat;
              -fx-background-position: 50%;"
              :padding (ui/insets :top 20
                                  :right 25
                                  :bottom 20
                                  :left 25)
             :on-action {:event :toggle-sidebar})]
          []
          (if sidebar-visible?
            (map (fn [m]
              (ui/button
                :font main-font
                :v-box/vgrow javafx.scene.layout.Priority/ALWAYS
                :style-class ["button1"]
                :text (clojure.string/replace (name m) #"-" " ")
                :on-action {:event :switch-current-view
                            :to m})) 
             menu-options)
            [])))))

(defui StatusBar
  (render [this state]
    (ui/h-box
      :children [(ui/label
                   :text "LEFT"
                   :font (:main-font state) 
                   :h-box/hgrow javafx.scene.layout.Priority/ALWAYS)
                 (ui/pane
                   :h-box/hgrow javafx.scene.layout.Priority/ALWAYS)
                 (ui/label
                   :font (:main-font state) 
                   :text "RIGHT"
                   :h-box/hgrow javafx.scene.layout.Priority/NEVER)])))

(defui MainWindow
       (render [this state]
          (ui/v-box
            :fill-width true
            :style-class ["root"]
            
            :children
            [(menubar/menu-bar state)
             (buttonbar/button-bar state)
             (mainview/main-view state)
             (status-bar state)])))

(defui Stage
       (render [this args]
         (ui/stage
           :title "java architecture tool"
           :min-height 800
           :min-width  600
           :listen/height {:event :height-change
                           :fn-fx/include {::new-item #{:text}}}
           :listen/width {:event :width-change
                          :fn-fx/include {::new-item #{:text}}}
           :shown true
           :on-shown {:event :app-start
                      :fn-fx/include {:fn-fx/event #{:target}}}
           :always-on-top true
           :scene (ui/scene
                    :stylesheets ["main.css"]
                    :on-key-pressed {:event :key-was-pressed-on-scene
                                     :fn-fx/include 
                                       {:fn-fx/event 
                                         #{:target :text :code
                                           :control-down? :shift-down? 
                                           :meta-down? :alt-down?}}}
                    :root (main-window args)))))

(defmulti handle-event (fn [state event]
                          (:event event)))

(defmethod handle-event :width-change
  [state event]
  (assoc state :width (event :fn-fx.listen/new)))

(defmethod handle-event :height-change
  [state event]
  (assoc state :height (event :fn-fx.listen/new)))

(defmethod handle-event :main-container-width-change
  [state event]
  (assoc-in state [:main-view :main-container :width] (event :fn-fx.listen/new)))

(defmethod handle-event :main-container-height-change
  [state event]
  (assoc-in state [:main-view :main-container :height] (event :fn-fx.listen/new)))


(defmethod handle-event :toggle-sidebar
  [state event]
  (update state :sidebar-visible? not))

(defmethod handle-event :key-was-pressed-on-scene 
  [state event]
  (let [character (-> event :fn-fx/includes :fn-fx/event :code)
        control-down? (-> event :fn-fx/includes :fn-fx/event :control-down?) 
        alt-down? (-> event :fn-fx/includes :fn-fx/event :alt-down?) 
        toggle-fullscreen? (= javafx.scene.input.KeyCode/F character)
        load-images? (= javafx.scene.input.KeyCode/L character)
        left? (= javafx.scene.input.KeyCode/LEFT character)
        right? (= javafx.scene.input.KeyCode/RIGHT character)
        ui-scale-up? (and control-down? alt-down?
                      (= javafx.scene.input.KeyCode/PLUS character))
        ui-scale-down? (and control-down? alt-down?
                        (= javafx.scene.input.KeyCode/MINUS character))]

    (println (= javafx.scene.input.KeyCode/ESCAPE character))

    (if (= javafx.scene.input.KeyCode/ESCAPE character) 
      (javafx.application.Platform/runLater
        (fn [] (let [target (-> event :fn-fx/includes :fn-fx/event :target)]
                 (if (= javafx.scene.Scene (type target))
                   (-> target .getWindow .close)
                   (-> target .getScene .getWindow .close))))))

    (-> state
        (assoc :shut-down? (= javafx.scene.input.KeyCode/ESCAPE character))
        (update :fullscreen? #(if toggle-fullscreen? (not %) %))
        (update :font-size #(if ui-scale-up? (inc (inc %)) %))
        (update :font-size #(if (and ui-scale-down? (< 2 %)) (dec (dec %)) %))
        ((fn [s] 
           (assoc s :main-font 
                  (if (or ui-scale-down? ui-scale-up?) 
                    (ui/font :family "Helvetica" :size (:font-size s)) 
                    (:main-font s)))))
        ((fn [s] 
           (assoc s :main-fx-font 
                  (if (or ui-scale-down? ui-scale-up?) 
                    (javafx.scene.text.Font. "Helvetica" (:font-size s)) 
                    (:main-fx-font s))))))))

(defmethod handle-event :swap-status
  [state {:keys [idx]}]
  (update-in state [:todos idx :done?] (fn [x]
                                         (not x))))

(defmethod handle-event :switch-current-view
  [state {:keys [to]}]
  (println "switching view to:" to)
  (assoc state :current-view to))

(defmethod handle-event :delete-item
  [state {:keys [idx]}]
  (update-in state [:todos] (fn [itms]
                              (println itms idx)
                              (vec (concat (take idx itms)
                                           (drop (inc idx) itms))))))

(defmethod handle-event :package-view-scroll
  [state event]
  (let [dy (-> event :fn-fx/includes :fn-fx/event :delta-y)]
    (update state :zoom-level #(* % (+ 1 (/ dy 400))))))

(defmethod handle-event :search-for
  [state event]
  (try
    (let 
      [text (-> event :fn-fx/includes :search-bar :text)
       _ (println "constructing package-view. input text is:")
       _ text
       _ (println "event was:")
       _ (clojure.pprint/pprint event)
       _ (println "calling fn...")
       graph (jat/package-listing-minimal-feedbackarc-ordered text)
       new-state (assoc state :current-package-view graph)
       _ (println "fn returned!")
       _ (clojure.pprint/pprint graph)]
      new-state)
    (catch Exception e (str "caught exception: " (.getMessage e)))))

(defmethod handle-event :default
  [state event]
  (println "No handler for event " (:type event) event)
  state)

(defn rand-mat [n]
  (let [rm (vec (map vec (partition n (map (fn [_] (int (rand 10))) (range (* n n))))))]
    (reduce (fn [acc v] (assoc-in acc [v v] 0)) rm (range n))))

(def default-state
  {:main-font (ui/font :family "Helvetica" :size 16)
   :main-fx-font (javafx.scene.text.Font. "Helvetica" 16)
   :font-size 16
   :zoom-level 1
   :packageview-translate-x 0.0
   :packageview-translate-y 0.0
   :current-view :something-else?
   :view-options [:package-viewer
                  :chord-diagram-view
                  :something-else?]
   :chord-diagram {:matrix (rand-mat 7)
                   :nodes (range 7)}
   :main-view {:main-container {:width 100 :height 100}}
   :current-package-view {:nodes ["dummy1"
                                  "dummy2"]
                          :edges [["dummy1" "dummy2"]
                                  ["dummy1" "dummy1"]
                                  ["dummy2" "dummy1"]]} 
   
   })

(def peekabo [])
(defn -main []
  (let [;; Data State holds the business logic of our app
        _ (jat/init-db-connection!)
        _ (styling/generate-css)
        data-state (atom default-state)
          
        _ (def peekabo data-state)
        ;; handler-fn handles events from the ui and updates the data state
        handler-fn (fn [event]
                     (try
                       (swap! data-state handle-event event)
                       (catch Throwable ex
                         (println ex))))

        ;; ui-state holds the most recent state of the ui
        ui-state   (agent (dom/app (stage @data-state) handler-fn))]

    ;; Every time the data-state changes, queue up an update of the UI
    (add-watch data-state :ui (fn [_ _ _ _]
                                (send ui-state
                                      (fn [old-ui]
                                        (try
                                          (dom/update-app old-ui (stage @data-state))
                                          (catch Throwable ex
                                            (do
                                              (clojure.stacktrace/print-throwable ex)
                                              (println ex))))))))))

(comment
  (clojure.pprint/pprint @peekabo)
  (-main))
