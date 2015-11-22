#lang racket

(define (string-repeat str n)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (string-append res str))))
  (help 0 ""))

(define (repeater str)
  (lambda (count glue)
    (string-append str (string-repeat (string-append glue str) (- count 1)))))
