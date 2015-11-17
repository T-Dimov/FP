#lang racket
(define (prime? n)
  (define (sum-divisors s i p)
    (if (> i p)
        s
        (if (= (remainder n i) 0)
            (sum-divisors (+ s i) (+ i 1) p)
            (sum-divisors s (+ i 1) p)))
  )
  (= (sum-divisors (+ n 0) 1 (quotient n 2)) (+ n 1))
)
