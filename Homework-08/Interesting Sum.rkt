#lang racket

(define (sum-divisors n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [(= (remainder n i) 0) (sum-iter (+ i 1) (+ result i))]
          [else (sum-iter (+ i 1) result)]))
  (sum-iter 1 0))

(define (sum-div n)
  (- (sum-divisors n) n))

(define (sum-interesting k)
  (cond
    [(= k 1) 0]
    [(= (sum-div (sum-div k)) k) (+ k (sum-interesting (- k 1)))]
    [else (sum-interesting (- k 1))]))
