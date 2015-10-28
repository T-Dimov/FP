#lang racket
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (add-frac frac1 frac2)
  (simplify-frac (if (= (snd frac1) (snd frac2))
      (cons (+ (fst frac1) (fst frac2)) (snd frac1))
      (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2)))))
  )

(define (substract-frac frac1 frac2)
  (simplify-frac (if (= (snd frac1) (snd frac2))
      (cons (- (fst frac1) (fst frac2)) (snd frac1))
      (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2)))))
  )

(define (mult-frac frac1 frac2)
  (simplify-frac (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))))
  )

(define (simplify-frac frac)
  (cons (/ (fst frac) (gcd (fst frac) (snd frac))) (/ (snd frac) (gcd (fst frac) (snd frac))))
  )
