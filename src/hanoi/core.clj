(ns hanoi.core
  (:gen-class))

(defn make-tower [size]
  (letfn [(make-tower-iter [size acc]
            (if (zero? size)
              acc
              (make-tower-iter (dec size)
                               ;; This cons converts vector to a list
                               (cons size acc))))]
    (make-tower-iter size [])))

;; vector as top-level b/c it works nice with assoc
;; lists as nested data structures b/c they work w/ cons
(def board [(make-tower 5) () ()])

;; I want to define the hanoi function such that it makes a bunch of calls to this function, all
;; in the correct order. This function, will allow us to watch the problem be solved step by step.
;; It's proving difficult, however. hanoi wants to be written in terms of "towers" not "indexes"
(defn move
  ([src dest]
   (move src dest board))

  ([src dest board]
   (let [src-tower (nth board src)
         dest-tower (nth board dest)]
     (let [new-state
           (assoc board
             src (rest src-tower)
             dest (cons (first src-tower) dest-tower))]
       (println new-state)
       new-state))))

(defn hanoi-move-seq [count src dest intermediate]
  (if (> count 0)
    (concat
      (hanoi-move-seq (- count 1) src intermediate dest)
      `((move ~src ~dest))
      (hanoi-move-seq (- count 1) intermediate dest src))
    '()))

(defmacro hanoi [count src dest intermediate]
  `(->> board ~@(hanoi-move-seq count src dest intermediate)))
