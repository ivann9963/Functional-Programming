#lang racket
(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (g x) (* 2 x x))

(define (derive2 f eps)
  (derive (derive f eps) eps))

(define g-s (derive2 g 0.0001))

;(g-s 2) - втората производна на g в точката 2

(define (derive-n f n eps)
  (if (= n 0) f
      (derive (derive-n f (- n 1) eps) eps)))

;((derive-n g 3 0.0001) 10)

(define (count-divisors n)
  (define (helper counter d)
    (cond
      [(equal? d (+ 1 n))   counter]
      [(= 0 (remainder n d)) (helper (+ 1 counter) (+ 1 d))]
      [else (helper counter (+ 1 d))]))
    (helper 0 1))

(define (prime? n)
  (if (= (count-divisors n) 2) #t #f))

(define (cubicHelperList n)
  (define (iter i n)
    (if (= i n) (list (expt i 3))
        (cons (expt i 3) (iter (+ i 1) n))))
  (iter 1 n))

(define (cubicDiffList n)
  (define (iter i n)
    (if (= i (- n 1)) (list (- (list-ref (cubicHelperList n) i) (list-ref (cubicHelperList n) (- i 1))))
        (cons (- (list-ref (cubicHelperList n) i) (list-ref (cubicHelperList n) (- i 1))) (iter (+ 1 i) n))))
  (iter 1 n))

(define lst (filter prime? (cubicDiffList 330)))

(define (nth-cuban n)
  (list-ref lst (- n 1)))

