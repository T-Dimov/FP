#lang racket
(define (count-cube-sums from to)
(define (cube-sums? n c)
  (define (cube a) (* a a a))
  (define (help i)
    (define (help1 j)
      (if (= (+ (cube i) (cube j)) n)
          #t
          (if (= j i)
              #f
              (help1 (+ j 1))))
    )
    (if (= n i)
        #f
        (if (help1 1)
            #t
            (help (+ i 1))))
  )
  (if (> n to)
      c
      (if (help 1)
          (cube-sums? (+ n 1) (+ c 1))
          (cube-sums? (+ n 1) c)))
)
  (cube-sums? from 0)
)
