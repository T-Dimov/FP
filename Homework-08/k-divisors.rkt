#lang racket

(define (sum-divisors n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [(= (remainder n i) 0) (sum-iter (+ i 1) (+ result i))]
          [else (sum-iter (+ i 1) result)]))
  (sum-iter 1 0))

(define (prime? n)
  (= (sum-divisors n) (+ n 1)))

(define (prime-divisors n)
  (define (help i res)
    (cond
      [(= i 1) res]
      [(and (prime? i) (= (remainder n i) 0)) (help (- i 1) (cons i res))]
      [else (help (- i 1) res)]))
  (help n (list)))

(define (k-divisors n k)
  (define (help i res)
    (cond
      [(= i 0) res]
      [(= (length (prime-divisors i)) k) (help (- i 1) (add1 res))]
      [else (help (- i 1) res)]))
  (help n 0))
