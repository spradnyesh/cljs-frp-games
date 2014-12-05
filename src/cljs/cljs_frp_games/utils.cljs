(ns cljs-frp-games.utils)

;; get n random indexes [of a 1D-array]
(defn- get-random-indexes [size n]
  (loop [n n
         acc #{}]
    (if (zero? n)
      (vec acc)
      (let [index (rand-int size)]
        (if (some #{index} acc)
          (recur n acc)
          (recur (dec n) (conj acc index)))))))

;; select n elements at random from arr
;; does ***NOT*** work on lazy sequences
(defn select-random [arr n]
  (let [size (count arr)]
    (map (partial get arr) (get-random-indexes size n))))
