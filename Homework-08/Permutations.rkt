#lang racket

(define (list-digits n)
  (cond
    [(= (quotient n 10) 0) (list (remainder n 10))]
    [else (cons (remainder n 10) (list-digits (quotient n 10)))]))

(define (all-permutations? items)
  (cond
    [(= (length items) 1) #t]
    [(equal? (sort (list-digits (first items)) <) (sort (list-digits (second items)) <)) (all-permutations? (rest items))]
    [else #f]))
