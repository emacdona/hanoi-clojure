(ns hanoi.core
  (:gen-class))

;; Make a tower for our board, eg: (1 2 3 4 5)
(defn make-tower [size]
  (letfn [(make-tower-iter [size acc]
            (if (zero? size)
              acc
              (make-tower-iter (dec size)
                               ;; This cons converts vector to a list
                               (cons size acc))))]
    (make-tower-iter size [])))

;; The board.
;; Vector as top-level b/c it works nice with assoc.
;; Lists as nested data structures b/c they work w/ cons.
(def board [(make-tower 5) () ()])

(def ^:dynamic *log-moves* true)

;; How we think of a "move" in the game.
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
       (if *log-moves* (println new-state))
       new-state))))

;; Function that generates a sequence of moves that solves the game.
;;
;; Note that this is a function of the count of pieces, not the board, nor "towers" on the board.
;; This has nothing to do with "to move a tower of size n, first move the tower of size n-1 on top of it".
;; THIS is why hanoi solutions are so confusing. The algorithm doesn't match the words they use to describe it.
;;
;; Also note how this algorithm gives no real evidence that it works. It doesn't show a sequence of state changes
;; to the board, nor does it modify the board in any way (it doesn't even know it exists). It simply generates
;; a program that will provide a set of steps to change the state of the board.
;;
;; Also note how hard it would be to actually write this so it WAS a function of the board. "ref: 2" is the line where
;; you would actually perform a 'move' operation. The problem is that the new state of the board returned by this move
;; is needed in the call to hanoi-move-seq on "ref: 1"!
(defn hanoi-move-seq [count src dest intermediate]
  (if (> count 0)
    (concat
      (hanoi-move-seq (- count 1) src intermediate dest)    ;; ref: 1
      `((move ~src ~dest))                                  ;; ref: 2
      (hanoi-move-seq (- count 1) intermediate dest src))   ;; ref: 3
    '()))

;; This macro takes the set of moves provided by the hanoi-move-seq function and uses the clojure threading macro
;; to log a sequence of moves on the board.
;;
;; Note that if we could evaluate 'board' via a binding in the context where the macro was called, we could compute
;; count. HOWEVER, THIS IS A MACRO. In the body of the macro, 'board' evaluates to a piece of lisp syntax (a symbol, a
;; list, a vector, etc). This caused me a lot of confusion... because src, dest, and intermediate -- which are all passed
;; to the macro as numbers... evaluate -- in the macro context, and all other contexts -- to numbers. "board" evaluates
;; (in the macro context) to whatever symbol was passed in its place ("board", "myboard", "aboard", etc) -- unlike in a
;; function where it could evaluate to something like ['(1 2 3) '() '()].
(defmacro hanoi
  ([board count src dest intermediate]
   `(->> ~board
         ~@(hanoi-move-seq count
                           src dest intermediate)))

  ;; Now, if we agree that we simply want to state the number of pieces on the board (instead of creating the board
  ;; ourselves and then passing it to our macro/function)...
  ([count src dest intermediate]
   `(->> (quote [~(make-tower count) () ()])
         ~@(hanoi-move-seq count
                           src dest intermediate)))
  )

;; Instead of logging the different board states, accumulate them!
(defmacro accumulating-hanoi
  ([count src dest intermediate]
   (let [board (gensym)]
     `(binding [*log-moves* false]
        (:state-accumulator
          (as-> {:board (quote [~(make-tower count) () ()]) :state-accumulator '()} ~board
                ~@(map (fn [x]
                         `(let [new-state# ~(concat x `((:board ~board)))]
                            (assoc ~board
                              :board new-state#
                              :state-accumulator (concat (:state-accumulator ~board)
                                                         `(~new-state#)))))
                       (hanoi-move-seq count
                                       src dest intermediate))
                ))))))

;; So, what if I really don't want to have to pass 'count'? Well, I generate a lisp expression that is a macro call
;; and then evaluate it. I would argue that this is an "ok" use of eval since I'm evaluating all arguments to the
;; macro before calling eval. In other words, I'm making sure that this eval returns the same value, regardless of
;; context -- because I'm not leaving any symbols to be evaluated during the call to 'eval'.
;;
(defn hanoi-f [board]
  (eval `(hanoi (quote ~board) ~(count (nth board 0)) 0 2 1)))


