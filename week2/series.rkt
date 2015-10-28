#lang racket
(define (series a b n)
  (define (help c d i)
    (if (= i n) d (help d (+ c d) (+ i 1)))
    )
  (cond
    [(= n 1) a]
    [(= n 2) b]
    [else (help b (+ a b) 3)])
  )

(define (lucas n)
  (series 2 1 n)
  )

(define (fibonacci n)
  (series 1 1 n)
  )

(define (summed-member n)
  (+ (lucas n) (fibonacci n))
  )

(define (nth-lucas-sum n)
  (define (help i res)
    (if (> i n)
        res
        (help (+ i 1) (+ res (lucas i))))
    )
  (help 1 0)
  )

(define (nth-fibonacci-sum n)
  (define (help i res)
    (if (> i n)
        res
        (help (+ i 1) (+ res (fibonacci i))))
    )
  (help 1 0)
  )

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n))
  )
