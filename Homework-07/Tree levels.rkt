#lang racket

(define (mktree n l r)
  (list n l r))

(define (mkleaf x)
  (mktree x '() '()))

(define (empty-tree? tree)
  (empty? tree))

(define (height t)
  (cond
    [(empty-tree? t) 0]
    [else (+ 1 (max (height (left t)) (height (right t))))]))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (tree-level level tree)
  (cond
    [(empty-tree? tree) '()]
    [(= level 1) (list (root tree))]
    [else (append (tree-level (- level 1) (left tree))
                  (tree-level (- level 1) (right tree)))]))

(define (tree-levels tree)
  (define (help level res)
    (cond
      [(= level 0) res]
      [else (help (- level 1)
                  (cons (tree-level level tree) res))]))
  (help (height tree) '()))
