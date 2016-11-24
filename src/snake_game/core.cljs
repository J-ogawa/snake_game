(ns snake-game.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler register-sub subscribe dispatch dispatch-sync]]
            [goog.events :as events]))

(enable-console-print!)

(def initial-state {:contents []})

(register-handler
  :initialize
  (fn [db _]
    (merge db initial-state)))

(register-handler
  :add
  (fn [db _]
    (update-in db [:contents] #(cons (-> % count inc) %))))

(register-sub
  :contents
  (fn [db _]
    (reaction (:contents @db))))

(defn render-list
  []
  (let [elements (subscribe [:contents])]
    (fn []
      [:div
       [:button {:on-click #(dispatch [:add])} "+"]
       [:ul (for [x @elements] [:li {:style {:background-color "#eeeeee"}} x])]])))

(defn main
  []
  (dispatch-sync [:initialize])
  (reagent/render [render-list]
                  (js/document.getElementById "app")))

(main)
