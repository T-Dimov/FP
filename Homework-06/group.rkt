#lang racket

(define (take-while p items)
  (define (help res lst)
    (cond
      [(empty? lst) res]
      [(not (p (first lst))) res]
      [else (help (cons (first lst) res) (rest lst))]))
  (reverse (help (list) items))
    )

(define (drop-while p items)
  (define (help res)
    (cond
      [(empty? res) res]
      [(not (p (first res))) res]
      [else (help (rest res))]))
  (help items)
  )

(define (group input)
  (define (help res lst)
    (cond
      [(empty? lst) res]
      [else (help (cons (take-while (lambda (x) (equal? x (first lst))) lst) res)
                  (drop-while (lambda (x) (equal? x (first lst))) lst))]))
  (reverse (help (list) input)))
