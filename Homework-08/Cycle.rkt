#lang racket

(define (shift lst)
  (cons (last lst) (take lst (- (length lst) 1))))

(define (at lst n)
  (define (help i lst)
    (cond
      [(empty? lst) -1]
      [(= (first lst) n) i]
      [else (help (+ i 1) (rest lst))]))
  (help 0 lst))

(define (shifts lst n)
  (cond
    [(= n 0) lst]
    [else (shifts (shift lst) (- n 1))]))

(define (cycle times items)
  (lambda (n)
    (at (shifts items times) n)))
