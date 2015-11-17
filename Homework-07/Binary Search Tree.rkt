#lang racket

(define (mktree n l r)
  (list n l r))

(define (mkleaf x)
  (mktree x '() '()))

(define (empty-tree? tree)
  (empty? tree))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (bst-insert x tree)
  (cond
    [(empty-tree? tree) (mkleaf x)]
    [(< x (root tree)) (mktree (root tree)
                               (bst-insert x (left tree))
                               (right tree))]
    [else (mktree (root tree)
                  (left tree)
                  (bst-insert x (right tree)))]))

(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (root tree)) #t]
    [(< x (root tree)) (bst-element? x (left tree))]
    [else (bst-element? x (right tree))]))

(define (bst->list tree)
  (cond
    [(empty-tree? tree) '()]
    [else (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (define (ascending l)
    (cond
      [(= (length l) 1) #t]
      [(> (first l) (second l)) #f]
      [else (ascending (rest l))]))
  (ascending (bst->list tree)))
