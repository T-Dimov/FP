#lang racket

(require "Hack_Numbers.rkt")

(define (p-score n)
  (define (help res n)
    (if (palindrome? n)
        (+ res 1)
        (help (+ res 1) (+ n (reverse-int n))))
    )
  (help 0 n)
  )
