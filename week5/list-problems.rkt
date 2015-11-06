#lang racket

; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6
(define (sum numbers)
  (define (help num res)
    (if (empty? num)
        res
        (help (rest num) (+ res (first num)))))
  (help numbers 0)
  )

; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html
(define (member? x items)
  (define (help lst)
    (cond
      [(empty? lst) #f]
      [(equal? x (first lst)) #t]
      [else (help (rest lst))]))
  (help items)
  )

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length
(define (length2 items)
  (define (help res lst)
    (if (empty? lst)
        res
        (help (+ res 1) (rest lst))))
  (help 0 items)
  )

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref
(define (list-ref2 items n)
  (define (help i lst)
    (if (= i n)
        (first lst)
        (help (+ i 1) (rest lst))))
  (help 0 items)
  )

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range
(define (range2 a b)
  (define (help i res)
    (if (< i a)
        res
        (help (- i 1) (cons i res))))
  (help (- b 1) (list))
  )

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list

(define (build-list2 n f)
  (define (help i res)
    (if (< i 0)
        res
        (help (- i 1) (cons (f i) res))))
  (help (- n 1) (list))
  )
; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append
(define (append2 l1 l2)
  (define (help res lst)
    (if (empty? lst)
        res
        (help (cons (first lst) res) (rest lst))))
  (help l2 (reverse2 l1))
  )

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse
(define (reverse2 items)
  (define (help res lst)
    (if (empty? lst)
        res
        (help (cons (first lst) res) (rest lst))))
  (help  (list) items)
  )

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)
(define (take2 n items)
  (define (help i res lst)
    (if (= i n)
        res
        (help (+ i 1) (cons (first lst) res) (rest lst))))
  (if (> n (length items))
      items
      (reverse2 (help 0 (list) items)))
  )

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)
(define (drop2 n items)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (rest res))))
  (if (> n (length items))
      (list)
      (help 0 items))
  )

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()
(define (take-while p items)
  (define (help res lst)
    (cond
      [(empty? lst) res]
      [(not (p (first lst))) res]
      [else (help (cons (first lst) res) (rest lst))]))
  (reverse2 (help (list) items))
    )

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава истина за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)
(define (drop-while p items)
  (define (help res)
    (cond
      [(empty? res) res]
      [(not (p (first res))) res]
      [else (help (rest res))]))
  (help items)
  )

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)
(define (number->list n)
  (define (help num res)
    (if (= num 0)
        res
        (help (quotient num 10) (cons (remainder num 10) res))))
  (if (= n 0)
      (list 0)
      (help n (list)))
  )

; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123
(define (list->number ns)
  (define (help res lst)
    (if (empty? lst)
        res
        (help (+ (* res 10) (first lst)) (rest lst))))
  (help 0 ns)
  )
