#lang racket

(define (take-while p items)
  (define (help res lst)
    (cond
      [(empty? lst) res]
      [(not (p (first lst))) res]
      [else (help (cons (first lst) res) (rest lst))]))
  (reverse (help (list) items)))

(define (drop-while p items)
  (define (help res)
    (cond
      [(empty? res) res]
      [(not (p (first res))) res]
      [else (help (rest res))]))
  (help items))

(define (group input)
  (define (help res lst)
    (cond
      [(empty? lst) res]
      [else (help (cons (take-while (lambda (x) (equal? x (first lst))) lst) res)
                  (drop-while (lambda (x) (equal? x (first lst))) lst))]))
  (reverse (help (list) input)))

(define (run-length-encode input)
  (define (head str) (substring str 0 1))
  (define (tail str) (substring str 1))
  (define (help str count res)
    (cond
      [(= (string-length str) 1) (string-append res
                                                ((lambda (x) (if (not (= x 1)) (~a x) "")) count)
                                                (head str))]
      [(equal? (head str) (head (tail str))) (help (tail str) (add1 count) res)]
      [(= count 1) (help (tail str) 1 (string-append res (head str)))]
      [else (help (tail str) 1 (string-append res (~a count) (head str)))]))
  (help input 1 ""))

(define (string-repeat str n)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (string-append res str))))
  (help 0 ""))

(define (log10 n) (/ (log n) (log 10)))

(define (run-length-decode input)
  (define (len str res)
    (cond
      [(zero? (string-length str)) '(0 "")]
      [(char-numeric? (string-ref str 0)) (len (substring str 1)
                                               (+ (* res 10)
                                                  (- (char->integer (string-ref str 0))
                                                                (char->integer #\0))))]
      [else (cons res (string-ref str 0))]))
  (define (help str res)
    (let ([times (car (len str 0))]
          [letter (cdr (len str 0))])
      (cond
        [(zero? (string-length str)) res]
        [(= times 0) (help (substring str 1)
                           (string-append res
                                          (substring str 0 1)))]
        [else (help (substring str
                               (inexact->exact (+ (floor (log10 times))
                                                  2)))
                    (string-append res
                                   (string-repeat (string letter)
                                                  times)))])))
  (help input ""))
