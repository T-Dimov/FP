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

(define (tree-map f tree)
  (cond
    [(empty-tree? tree) '()]
    [else (mktree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))
