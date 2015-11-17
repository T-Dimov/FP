#lang racket

(require "fence.rkt")

(define (nth-beast-number n)
  (string->number (string-repeat "666" n)))
