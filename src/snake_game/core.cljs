(ns snake-game.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler register-sub subscribe dispatch dispatch-sync]]
            [goog.events :as events]))

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(def board [35 25])

(def snake
  {:direction [1 0]
   :body [[3 2] [2 2] [1 2] [0 2]]})

(defn free-positions
  [{:keys [body] :as snake}
   [width height]]
    (remove `#{~@body} (for [x (range width) y (range height)] [x y])))

(def initial-state
  {:board board
   :snake snake
   :point (rand-nth (free-positions snake board))
   :points 0
   :game-running? true
   :step 0})

(defn move-snake
  [{:keys [direction] :as snake}]
  (update-in snake [:body]
             (fn [[h & _ :as body]]
               `[~@(cons (mapv + direction h) (drop-last body))])))

(def key-code->move
  {38 [0 -1]
   40 [0 1]
   39 [1 0]
   37 [-1 0]})

(defonce snake-moving
  (js/setInterval #(dispatch [:next-state]) 150))

(defonce key-handler
  (events/listen
    js/window "keydown"
    (fn [e]
      (when-let [enable-key (key-code->move (.-keyCode e))]
        (dispatch [:change-direction enable-key])))))

(defn change-snake-direction
  [new_ old_]
  (if (some true? (map = new_ old_)) old_ new_))

(defn new-last [body]
  (let [[penultimate last_] (take-last 2 body)]
  (map #(+ %2 (- %2 %1)) penultimate last_)))

(defn grow-snake
  [snake]
  (update-in snake [:body] #(conj % (new-last %))))

(defn process-move
  [{:keys [snake point board] :as db}]
  (if (= point (first (:body snake)))
    (-> db
        (update-in [:snake] grow-snake)
        (update-in [:points] inc)
        (assoc     :point (rand-nth (free-positions snake board))))
    db))

(defn collision?
  [{[head & rbody] :body direction :direction} [x-size y-size]]
  (let [[future-x future-y :as future_] (map + direction head)]
    (or (contains? `#{~x-size -1} future-x)
        (contains? `#{~y-size -1} future-y)
        (contains? `#{~@rbody} future_))))

(register-handler
  :initialize
  (fn
    [db _]
    (merge db initial-state)))

(register-handler
  :next-state
  (fn
    [{:keys [snake board game-running? step points] :as db} _]
    (if game-running?
      (if (collision? snake board)
        (assoc-in db [:game-running?] false)
        (-> db
            (update-in [:step] inc)
            (update-in [:snake] move-snake)
            process-move))
      db)))

(register-handler
  :change-direction
  (fn [db [_ new-direction]]
    (update-in db [:snake :direction]
               (partial change-snake-direction new-direction))))

(register-sub
  :board
  (fn
    [db _]
    (reaction (:board @db))))

(register-sub
  :snake
  (fn
    [db _]
    (reaction (:body (:snake @db)))))

(register-sub
  :point
  (fn
    [db _]
    (reaction (:point @db))))

(register-sub
  :points
  (fn
    [db _]
    (reaction (:points @db))))

(register-sub
  :game-running?
  (fn
    [db _]
    (reaction (:game-running? @db))))

(defn render-board
  []
  (let [board (subscribe [:board])
        snake (subscribe [:snake])
        point (subscribe [:point])]
    (fn []
      (let [[width height]  @board
            snake-positions (set @snake)
            current-point   @point]
        [:table.stage {:style {:height 377 :width 527}}
         (for [y (range height)]
           [:tr
            (for [x (range width)]
              (cond
                (snake-positions [x y]) [:td.snake-on-cell]
                (= current-point [x y]) [:td.point]
                :else                   [:td.cell]))])]))))

(defn score
  []
  (let [points (subscribe [:points])]
    (fn []
      [:div.score (str "Score: " @points)])))

(defn game-over
  []
  (let [game-state (subscribe [:game-running?])]
    (fn []
      (if @game-state
        [:div]
        [:div.overlay
         [:div.play {:on-click #(dispatch [:initialize])}
          [:h1 "^"]]]))))

(defn game
  []
  [:div
   [render-board]
   [score]
   [game-over]])

(defn run
  []
  (dispatch-sync [:initialize])
  (reagent/render [game]
                  (js/document.getElementById "app")))

(run)
