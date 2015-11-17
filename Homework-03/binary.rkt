#lang racket

(provide to-binary-string)

(define (string-reverse str)
  (define (help i res n)
    (if (< i 0)
        res
        (help (- i 1) (string-append res (~a (string-ref str i))) n))
    )
  (help (- (string-length str) 1) "" (string-length str))
  )

(define (to-binary-string n)
  (define (help res cur)
    (if (= cur 0)
        res
        (help (string-append res (~a (remainder cur 2))) (quotient cur 2)))
    )
  (string-reverse (help "" n))
  )

(define (from-binary-string binary-str)
  (define (help res i n)
    (if (= i n)
        res
        (help (+ (* res 2) (string->number (~a (string-ref binary-str i)))) (+ i 1) n))
    )
  (help 0 0 (string-length binary-str))
  )
