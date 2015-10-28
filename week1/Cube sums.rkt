#lang racket
(define (cube-sums? n)
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
  (help 1)
)
