#lang racket

(define (sum-divisors n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [(= (remainder n i) 0) (sum-iter (+ i 1) (+ result i))]
          [else (sum-iter (+ i 1) result)])
  )
  (sum-iter 1 0)
)

(define (prime? n)
  (= (sum-divisors n) (+ n 1)))
  
(define (truncatable-prime? x)
  (cond
    [(< x 10) (prime? x)]
    [(prime? x) (truncatable-prime? (quotient x 10))]
    [else #f]))
