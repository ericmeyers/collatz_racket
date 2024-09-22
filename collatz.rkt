#lang racket

(require srfi/41)
(require plot)

(define (collatz n)
  (if (even? n)  (/ n 2) (+ 1 (* 3 n))))


;; lazy infinite list
(define (collatz-list2 n)
    (append (stream->list (stream-take-while (lambda (x) (> x 1)) (stream-iterate collatz n))) '(1)))


;; not lazy, not infinite
(define (collatz-list n)
  (cond
    [(= 1 n) '(1)]
    [(even? n)  (cons n (collatz-list (/ n 2)))]
    [#t         (cons n (collatz-list (+ 1 (* 3 n))))]))




(define xs (range 1 10001))
(define ys (time (map (compose length collatz-list) xs)))


(plot-new-window? #t)


(parameterize (
    [plot-font-size 20]
    [point-sym 'fullcircle5]
    [point-color "blue"]
    [plot-width 1200]
    [plot-height 800])

(plot #:title "Collatz Conjecture" #:x-label "Number" #:y-label "Sequence Length" (list (tick-grid) (points (map vector xs ys)))))
