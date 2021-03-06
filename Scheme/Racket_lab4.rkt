#lang racket

;5та задача от трето упражнение - inc-digits
(define (count-digits n)
  (define (counter c n)
    (if (< n 10) (+ 1 c)
        (counter (+ 1 c) (quotient n 10))))
  (counter 0 n))


(define (inc-digits? n)
  (define (iter res n)
    (cond
      [(= n 0) #t]
      [(< (remainder (quotient n 10) 10) (remainder n 10)) (iter res (quotient n 10))]
      [else #f]))
  (iter #f n))

(define (geomProgressionSum x n)
  (define (iter i x n res)
    (if (= i (+ 1 n)) res
        (+ (expt x i) (iter (+ 1 i) x n res))))
  (iter 0 x n 0))

;започваме задачите от четвърто упражнение

(define (suffix? a b)
    (if (equal? a (remainder b (expt 10 (count-digits a)))) #t #f))

(define (prefix? a b)
  (if (equal? a (quotient b (expt 10 (- (count-digits b) (count-digits a))))) #t #f))

(define (substr? a b)
  (if (suffix? a b) #t
           (substr? a (quotient b 10))))

(define (my-identity x) x)

(define next-1 (lambda (x) (+ x 4)))
(define (next x) (+ x 4))

; lambda == λ (Ctrl + \)

(define (my-compose f g)
  (λ (x) (f (g (x)))))

;((my-compose (λ (x) (+ 1 x)) (λ (x) (* x 2))) 5)
; -> връша 5*2 + 1 = 11

(define (my-negate p?) (λ (x) (not (p? x))))

(define (my-negate-c p?)
  (my-compose not p?))

(define (even? x)
  (= 0 (remainder x 2)))

(define (odd? n)
  ((my-negate even?) n))

;curry - частично прилагане

(define (f a b c)
  (+ a (* b c)))

(define f-1
  (curry f 1))

(define (f-1-2)
  (f-1 2))

(define (F a) (+ a 2))

(define (difference F a b)
  (- (F b) (F a)))

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (g x) (* 2 x x))
(define g-p (derive g 0.001))

; започваме със задачите от пето упражнение












