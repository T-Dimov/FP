#lang racket

(define (to-zero i lst)
  (define (help j left right)
    (cond
      [(= j i) (append left (list 0) (rest right))]
      [else (help (+ j 1) (append left (list (first right))) (rest right))]))
  (help 0 (list) lst))

(define (zeroes matrix)
  (define (help i res mat row)
    (cond
      [(empty? row) (if (empty? mat) res (help 0 res (rest mat) (first mat)))]
      [(= (first row) 0) (help (+ i 1) (cons i res) mat (rest row))]
      [else (help (+ i 1) res mat (rest row))]))
  (help 0 (list) (rest matrix) (first matrix)))

(define (zeroed i matrix)
  (cond
    [(empty? matrix) (list)]
    [else (cons (to-zero i (first matrix)) (zeroed i (rest matrix)))]))

(define (zero matrix)
  (define (cycle lst mat)
    (cond
      [(empty? lst) mat]
      [else (cycle (rest lst) (zeroed (first lst) mat))]))
  (let
      ([zeroes (zeroes matrix)])
      (cycle zeroes matrix)))
