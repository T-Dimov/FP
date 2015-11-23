#lang racket

(define (has-zero? lst)
  (cond
   [(empty? lst) #f]
   [(= (first lst) 0) #t]
   [else (has-zero? (rest lst))]))

(define (zeroes n)
  (cond
    [(= n 0) (list)]
    [else (cons 0 (zeroes (- n 1)))]))

(define (zero matrix)
  (cond
    [(empty? matrix) '()]
    [(has-zero? (first matrix)) (cons (zeroes (length (first matrix))) (zero (rest matrix)))]
    [else (cons (first matrix) (zero (rest matrix)))]))
