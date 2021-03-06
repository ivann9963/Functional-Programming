#lang racket
;Първа задача
(define (countdigits n)
  (define (helper n counter)
    (if (equal? n 0) counter
        (helper (quotient n 10) (+ 1 counter))))
    (helper n 0))

;(count-equal-end-digits a b) -> връща броят на равни цифри на а и б, отзад напред
(define (count-equal-end-digits a b)
  (define (counter n a b)
    (if (= (remainder a 10) (remainder b 10)) (counter (+ 1 n) (quotient a 10) (quotient b 10))
        n))
  (counter 0 a b))

(define (automorphic? n)
  (if (= (count-equal-end-digits (expt n 2) n) (countdigits n)) #t #f))

;Втора задача
(define (count-divisors n)
  (define (helper counter d)
    (cond
      [(equal? d (+ 1 n))   counter]
      [(= 0 (remainder n d)) (helper (+ 1 counter) (+ 1 d))]
      [else (helper counter (+ 1 d))]))
    (helper 0 1))

(define (prime? n)
  (if (= (count-divisors n) 2) #t #f))

; (cubicHelperList n) -> връща списък от кубовете на първите n числа
(define (cubicHelperList n)
  (define (iter i n)
    (if (= i n) (list (expt i 3))
        (cons (expt i 3) (iter (+ i 1) n))))
  (iter 1 n))

;(cubicDiffList n) -> връща разликите от кубовете на първите n числа (n-1 разлики)
(define (cubicDiffList n)
  (define (iter i n)
    (if (= i (- n 1)) (list (- (list-ref (cubicHelperList n) i) (list-ref (cubicHelperList n) (- i 1))))
        (cons (- (list-ref (cubicHelperList n) i) (list-ref (cubicHelperList n) (- i 1))) (iter (+ 1 i) n))))
  (iter 1 n))
 
;Създаваме списък само от тези кубични разлики, които са прости числа
(define lst (filter prime? (cubicDiffList 330)))

(define (nth-cuban n)
  (list-ref lst (- n 1)))