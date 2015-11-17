#lang racket
(define (circle? circle-x circle-y radius point-x point-y)
  (<= (+ (* (- point-x circle-x) (- point-x circle-x)) (* (- point-y circle-y) (- point-y circle-y))) (* radius radius))
  )
