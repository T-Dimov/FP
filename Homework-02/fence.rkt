#lang racket
(define (string-repeat str n)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (string-append res str)))
    )
  (help 0 "")
  )

(define (fence n)
  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (~a n) "<" (string-repeat "-" (round (+ 1 (log n)))) "}")
  )
