#lang racket

(define (i-th-column i M)
  (define (help j res mat row)
    (cond
      [(empty? row) (if (empty? mat)
                        res
                        (help 0 res (rest mat) (first mat)))]
      [(= j i) (if (empty? mat)
                   (cons (first row) res)
                   (help 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help (+ j 1) res mat (rest row))]))
  (reverse (help 0 (list) (rest M) (first M))))

(define (max-in-list lst)
  (first (sort lst >)))

(define (matrix-sum matrix)
  (define (help i res M)
    (let
        ([maximum (if (empty? M)
                      (void)
                      (max (max-in-list (i-th-column i matrix))
                           (max-in-list (first M))))])
      (cond
        [(empty? M) res]
        [else (help (+ i 1) (+ res maximum) (rest M))])))
  (help 0 0 matrix))
