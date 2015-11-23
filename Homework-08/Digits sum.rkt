#lang racket

(define (string-repeat str n)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (string-append res str))))
  (help 0 ""))

(define (max n)
  (string->number (string-repeat "1" n)))

(define (sum-digits n)
  (if (= (quotient n 10) 0)
      n
      (+ (remainder n 10) (sum-digits (quotient n 10)))))

(define (no-zeroes? n)
  (cond
    [(= n 0) #t]
    [(= (remainder n 10) 0) #f]
    [else (no-zeroes? (quotient n 10))]))

(define (digits-sum n)
  (define (help i sum)
    (cond
      [(= i 0) sum]
      [(and (no-zeroes? i) (= (sum-digits i) n)) (help (- i 1) (+ sum i))]
      [else (help (- i 1) sum)]))
  (cond
    [(or (< n 1) (> n 9)) "Invalid input!"]
    [else (help (max n) 0)]))
