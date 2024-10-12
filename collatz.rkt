#lang racket

(require plot)

(define (collatz n)
  (if (even? n)  (/ n 2) (+ 1 (* 3 n))))




(define (collatz-list n)
  (cond
    [(= 1 n) '(1)]
    [(even? n)  (cons n (collatz-list (/ n 2)))]
    [#t         (cons n (collatz-list (+ 1 (* 3 n))))]))


(define (collatz-brian n)
  (cond
    [(= 1 n) 1]
    [(even? n)  (+ 1 (collatz-brian (/ n 2)))]
    [#t         (+ 1 (collatz-brian (+ 1 (* 3 n))))]))


(define xs (inclusive-range 1 5000000))
(define ys (time (map collatz-brian xs)))


;(plot-new-window? #t)


(parameterize (
    [plot-font-size 20]
    [point-sym 'fullcircle2]
    [point-color "blue"]
    [plot-width 1200]
    [plot-height 800])

(plot #:out-file "myplot.png" #:title "Collatz Conjecture" #:x-label "Number" #:y-label "Sequence Length" (list (tick-grid) (points (map vector xs ys)))))
