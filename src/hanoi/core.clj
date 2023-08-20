(ns hanoi.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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

(defn all-but-last [list]
  (reverse (rest (reverse list))))

(def debug false)

(defn hanoi
  ([count src dest intermediate]
   (if (> count 0)
     (do
       (hanoi
         (- count 1)
         src intermediate dest)
       (move src dest)
       (hanoi
         (- count 1)
         intermediate dest src)
       )
     ))
  ([]
   (hanoi (count (first board))
          0 2 1)))

(defn hanoi-old
  ([board src-index dest-index intermediate-index src-tower dest-tower intermediate-tower indent]
   (let [top-src-tower (all-but-last src-tower)]
     (cond
       debug
       (println
         (format "src-index: %s\ndest-index: %s\nintermediate-index: %s\nsrc-tower: %s\ndest-tower: %s\nintermediate-tower: %s\n\n"
                 src-index dest-index intermediate-index src-tower dest-tower intermediate-tower)))
     (cond
       (> (count src-tower) 0)
       (do
         (hanoi-old board
                    src-index dest-index intermediate-index
                    top-src-tower intermediate-tower dest-tower)

         (hanoi-old (move src-index dest-index board)
                    intermediate-index dest-index src-index
                    top-src-tower (nth board dest-index) intermediate-tower)
         )

       true board
       )))

  ([board src-index dest-index intermediate-index src-tower dest-tower intermediate-tower]
   (hanoi-old board
              src-index
              dest-index
              intermediate-index
              src-tower
              dest-tower
              intermediate-tower
              "")
   )

  ([board src-index dest-index intermediate-index]
   (hanoi-old board
              src-index
              dest-index
              intermediate-index
              (nth board src-index)
              (nth board dest-index)
              (nth board intermediate-index))
   )

  ([src-index dest-index intermediate-index]
   (hanoi-old board
              src-index
              dest-index
              intermediate-index
              (nth board src-index)
              (nth board dest-index)
              (nth board intermediate-index))
   )
  ([]
   (hanoi-old 0 1 2)))
