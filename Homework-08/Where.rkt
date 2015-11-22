#lang racket

(define (where list-elements list-predicates)
  (cond
    [(empty? list-elements) (list)]
    [(empty? list-predicates) list-elements]
    [else (where (filter (first list-predicates) list-elements) (rest list-predicates))]))
