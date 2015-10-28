#lang racket

(provide palindrome?)
(provide reverse-int)
(require "binary.rkt")

(define (last-digit n) (remainder n 10))

(define (reverse-int n)
  (define (rev-iter n result)
    (cond [(= n 0) result]
          [else (rev-iter (quotient n 10) (+ (* result 10) (last-digit n)))]))
  (rev-iter n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (occurrences a n)
  (define (occ-iter n result)
    (cond [(= n 0) result]
          [(= (remainder n 10) a) (occ-iter (quotient n 10) (+ result 1))]
          [else (occ-iter (quotient n 10) result)]))
  (occ-iter n 0))

(define (next-hack n)
  (define (is-hack? n)
    (and (palindrome? (string->number (to-binary-string n))) (= (remainder (occurrences 1 (string->number (to-binary-string n))) 2) 1))
    )
  (define (help i)
    (if (is-hack? i)
        i
        (help (+ i 1)))
    )
  (help (+ n 1))
  )
