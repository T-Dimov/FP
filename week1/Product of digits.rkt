#lang racket
(define (product-digits n)
  (if (= (quotient n 10) 0)
      n
      (* (remainder n 10) (product-digits (quotient n 10))))
  )
