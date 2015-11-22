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

(define (main-diag M)
  (define (help i j res mat row)
    (cond
      [(empty? row) res]
      [(= i j) (if (empty? mat)
                   (cons (first row) res)
                   (help (+ i 1) 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help i (+ j 1) res mat (rest row))]))
  (reverse (help 0 0 (list) (rest M) (first M))))

(define (secondary-diag M)
  (define (help i j res mat row)
    (cond
      [(empty? row) res]
      [(= (+ i j) (- (length M) 1)) (if (empty? mat)
                                       (cons (first row) res)
                                       (help (+ i 1) 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help i (+ j 1) res mat (rest row))]))
  (reverse (help 0 0 (list) (rest M) (first M))))

(define (rows? M)
  (cond
    [(= (length M) 1) #t]
    [(= (foldl + 0 (first M)) (foldl + 0 (first (rest M)))) (rows? (rest M))]
    [else #f]))

(define (columns? M)
  (define (help i)
    (cond
      [(= i (- (length M) 1)) #t]
      [(= (foldl + 0 (i-th-column i M)) (foldl + 0 (i-th-column (+ i 1) M))) (help (+ i 1))]
      [else #f]))
  (help 0))

(define (magic-square? M)
  (and (rows? M)
       (columns? M)
       (= (foldl + 0 (i-th-column 0 M)) (foldl + 0 (main-diag M)))
       (= (foldl + 0 (i-th-column 0 M)) (foldl + 0 (secondary-diag M)))))
