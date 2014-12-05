(ns cljs-frp-games.game-of-life)

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; game logic

(defn init-random-row [size]
  (map #(rand-int 2) (range size)))

(defn init-random-board [size]
  (map #(init-random-row size) (range size)))

(defn w [b x y l]
  (if (and (>= x 0) (< x l)
           (>= y 0) (< y l))
    (nth (nth b x) y)
    0))

(defn get-neighbor-weight [b x y l]
  (reduce + [(w b (dec x) (dec y) l) (w b x (dec y) l) (w b (inc x) (dec y) l)
             (w b (dec x) y l)                         (w b (inc x) y l)
             (w b (dec x) (inc y) l) (w b x (inc y) l) (w b (inc x) (inc y) l)]))

;; rules at http://en.wikipedia.org/wiki/Conway's_Game_of_Life#Rules
(defn calculate-new-cell-weight [board x y limit]
  (let [new-wt (get-neighbor-weight board x y limit)
        wt (w board x y limit)]
    (cond (and (= 1 wt) (< new-wt 2)) 0 ; rule-1: die by under-population
          (and (= 1 wt) (or (= 2 new-wt) (= 3 new-wt))) 1 ; rule-2: live onto next generation
          (and (= 1 wt) (> new-wt 3)) 0 ; rule-3: die by over-population
          (and (zero? wt) (= new-wt 3)) 1 ; rule-4: live by reproduction
          :else wt))) ; 0 actually

(def draw-board nil) ; needed for recursion w/ update-board
(defn update-board [board]
  (draw-board (let [size (count board)]
                (map (fn [x] (map #(calculate-new-cell-weight board x % size)
                                  (range size)))
                     (range size)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view

(def canvas (.getElementById js/document "canvas"))
(def dead "#eee")
(def alive "#555")
(def cell-size 10) ; (in px)
(def margin 4) ; space b/n cells (in px)
(def update-time 1000)

(defn draw-cell [x y color]
  (let [context (.getContext canvas "2d")
        x-pos (* x (+ cell-size margin))
        y-pos (* y (+ cell-size margin))]
    (set! (. context -fillStyle) color)
    (.fillRect context x-pos y-pos cell-size cell-size)))

(defn binary->color [binary]
  (if (zero? binary) dead alive))

(defn draw-board [board]
  (let [size (count board)]
    (dorun (map (fn [x]
                  (dorun (map #(draw-cell x % (binary->color (nth (nth board x) %)))
                              (range size))))
                (range size)))
    (js/setTimeout (fn [] (update-board board))
                   update-time)))
