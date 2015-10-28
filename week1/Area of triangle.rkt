```racket
#lang racket
(define (area a b c)
(/ (sqrt(* (+ a b c) (- (+ c b) a) (- (+ a c) b) (- (+ a b) c))) 4)
  )
```
