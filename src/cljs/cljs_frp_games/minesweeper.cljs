(ns cljs-frp-games.minesweeper
  (:require [cljs-frp-games.utils :as u]
            [clojure.set :as set]))

;; board structure
;; each cell is a vector of [mine-present? #neighboring-mines state pin-present?]
;; where
;; ___-present?: 0 => absent, 1 => present
;; state: 0 => closed, 1 => open (showing #neighboring-mines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; getters

(defn get-cell [board x y]
  (nth (nth board x) y))

(defn mine-present? [cell]
  (= 1 (first cell)))

(defn neighboring-mines [cell]
  (second cell))

(defn state [cell]
  (nth cell 2))

(defn pin-present? [cell]
  (= 1 (nth cell 3)))

(defn get-neighbor-locations [x y]
  (map (fn [[a b] [x y]] [(+ x a) (+ y b)])
       [[-1 -1] [0 -1] [1 -1]
        [-1 0]         [1 0]
        [-1 1]  [0 1]  [1 1]]
       (repeat [x y])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initialize board

(defn init-mines [bx by nm]
  (u/select-random (vec (for [x (range bx) y (range by)]
                          [x y]))
                   nm))

(defn init-blank-board [bx by]
  (vec (map (fn [_] (vec (take bx (repeat [0 0 0 0])))) ; ***init-board-structure***
            (range by))))

;; update mine-present? to 1 whereever mines are present
(defn build-mine-board [board mines]
  (reduce (fn [a b] (assoc-in a (conj b 0) 1))
          board
          mines))

;; does cell have mine?
(defn w [b x y bx by]
  (if (and (>= x 0) (< x bx)
           (>= y 0) (< y by))
    (mine-present? (get-cell b x y))
    0))

;; calculate #neighboring-mines for cell xy
(defn get-cell-weight [b x y bx by]
  (reduce + [(w b (dec x) (dec y) bx by) (w b x (dec y) bx by) (w b (inc x) (dec y) bx by)
             (w b (dec x) y bx by)                             (w b (inc x) y bx by)
             (w b (dec x) (inc y) bx by) (w b x (inc y) bx by) (w b (inc x) (inc y) bx by)]))

;; calculate #neighboring-mines for whole board
(defn build-weight-board [board]
  (let [bx (count board)
        by (count (board 0))]
    (loop [x (dec bx)
           brd board]
      (if (= x -1) brd
          (recur (dec x) (loop [y (dec by)
                                b brd]
                           (if (= y -1) b
                               (recur (dec y)
                                      (assoc-in b [x y 1] (get-cell-weight board x y bx by))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view

(def board (atom nil))
(def canvas (.getElementById js/document "canvas"))
(def cell-size 25)
(def margin 1)
(def text "#111")
(def close "#aaa")
(def open "#eee")
(def die "#f00")
(def pin "#555")

(defn draw-cell [x y color nh]
  (let [context (.getContext canvas "2d")
        x-pos (* x (+ cell-size margin))
        y-pos (* y (+ cell-size margin))]
    (set! (. context -fillStyle) color)
    (.fillRect context x-pos y-pos cell-size cell-size)
    (when (and (= color close) (not (zero? nh)))
      (let [size (.round js/Math (/ cell-size 2))]
        (set! (. context -font) (str size "px sans-serif"))
        (set! (. context -textAlign) "center")
        (set! (. context -textBaseline) "middle")
        (set! (. context -fillStyle) text)
        (.fillText context (str nh) (+ size 1 x-pos) (+ size 1 y-pos))))))

(defn binary->color [[mine-present? neighboring-mines state pin-present?]]
  (cond (zero? state) close
        (= 1 mine-present?) die
        (= 1 pin-present?) pin
        :else open))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; game logic

(defn game-over? [board]
  (let [bx (count board)
        by (count (board 0))]
    (every? true? (vec (for [x (range bx), y (range by)
                             :let [cell (get-cell board x y)]]
                         (= (mine-present? cell) (pin-present? cell)))))))

(defn draw-board [board]
  (println board)
  (if (game-over? board)
    (js/alert "Congratulations! You won :)")
    (let [bx (count board)
          by (count (board 0))]
      (dorun (map (fn [x]
                    (dorun (map #(let [cell (get-cell board x %)]
                                   (draw-cell x % (binary->color cell) (second cell)))
                                (range by))))
                  (range bx)))
      board)))

(defn set-rest [set element]
  (set/difference set #{element}))

(defn open-n-locs [board n-locs]
  (if (empty? n-locs)
    board
    (let [[x y] (first n-locs)
          cell (try (get-cell board x y)
                    (catch js/Error e
                      (open-n-locs board
                             (set-rest n-locs [x y]))))]
      ;; was closed (ignore already open cells, because their n-locs have already been processed)
      (when (= 0 (state cell))
        (if (zero? (neighboring-mines cell))
          (recur (assoc-in board [x y 2] 1)
                 (set/union (set-rest n-locs [x y])
                            (get-neighbor-locations x y))) ; neither BFS nor DFS (union -> random order)
          (recur (assoc-in board [x y 2] 1)
                 (set-rest n-locs [x y])))))))

(defn open-cell [x y]
  (println "opening cell: " x ", " y)
  (let [board @board
        cell (get-cell board x y)]
    (cond (mine-present? cell)
          (throw (js/Error. "mine-field blasted"))

          ;; do nothing for an already open cell
          (= 1 (state (get-cell board x y)))
          board

          :else
          (if (> (neighboring-mines cell) 0)
            (reset! board (draw-board (assoc-in board [x y 2] 1)))
            (reset! board (draw-board (open-n-locs (assoc-in board [x y 2] 1)
                                                   (into #{} (get-neighbor-locations x y)))))))))

(defn pin-cell [x y]
  (println "pinning cell: " x ", " y)
  (let [board @board]
    (if (= 1 (state (get-cell board x y)))
      (throw (js/Error. "cannot pin an open cell"))
      (reset! board (draw-board (assoc-in board [x y 3] 1))))))

(defn board-xy->cell-xy [x y]
  (let [size (+ cell-size margin)]
    [(.floor js/Math (/ x size)) (.floor js/Math (/ y size))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; event handling
(defn get-mouse-pos [canvas event]
  (let [rect (.getBoundingClientRect canvas)]
    [(- (. event -clientX) (. rect -left)) (- (. event -clientY) (. rect -top))]))

(defn do-the-do [event]
  (let [[x y] (apply board-xy->cell-xy (get-mouse-pos canvas event))]
    (cond (= 0 (. event -button)) ; left-click
          (open-cell x y)
          (= 2 (. event -button)) ; right-click
          (pin-cell x y)
          :else nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; game interaction

(defn init-random-board [bx by nm]
  (.addEventListener canvas "mousedown" do-the-do)
  (build-weight-board (build-mine-board (init-blank-board bx by) (init-mines bx by nm))))
