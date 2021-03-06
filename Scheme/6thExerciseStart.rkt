#lang racket
(define (f x)
  (lambda (y) (+ x y))
  )

(define g (f 3))

(define (generate a b)
  (lambda (x) (+ x a b)))

;((generate 10 20) 2)

; f n -> (f (f (f ... (f k) ... )))

(define (repeated f n)
  (define (helper f n x)
    (if (= n 0) x
        (f (helper f (- n 1) x))))
  (λ (x) (helper f n x)))

((repeated (λ (x) (+ x 1)) 5) 3)
; -> горния ред връща 3+1+1... и така 5 пъти -> 8

;1 задача
(define (list-length lst)
  (define (counter n lst)
    (if (null? lst) n
        (counter (+ n 1) (cdr lst))))
  (counter 0 lst))

(define (list-el? lst el)
  (define (iter i el lst)
    (cond
      [(null? lst) #f]
      [(= i el) #t]
      [else (iter (car lst) el (cdr lst))]))
  (iter (car lst) el lst))

(define (addElement el pos lst)
  (define (iter i

