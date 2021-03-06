#lang racket


(define (countdigits n)
  (define (helper n counter)
    (if (equal? n 0) counter
        (helper (quotient n 10) (+ 1 counter))))
    (helper n 0))

(define (reverse-number2 n)
  (define (helper res k)
    (if (< k 10)
        (+ (* res 10) k)
        (helper (+ (remainder k 10) (* res 10)) (quotient k 10))))
  (helper 0 n))

(define (reverse-number n)
  (define (helper k countDigitsK)
    (if (< k 10)
        k
        (+
         (* (remainder k 10) (expt 10 (- countDigitsK 1)))
         (helper (quotient k 10) (- countDigitsK 1)))))
  (helper n (countdigits n)))

(define (palindrome? n)
  (= n (reverse-number n)))

(define (count-palindromes a b)
    (cond
      [(> a b) 0]
      [(palindrome? a) (+ 1 (count-palindromes (+ a 1) b))]
      [else (count-palindromes (+ 1 a) b)]))

(define (count-divisors n)
  (define (helper counter d)
    (cond
      [(equal? d (+ 1 n))   counter]
      [(= 0 (remainder n d)) (helper (+ 1 counter) (+ 1 d))]
      [else (helper counter (+ 1 d))]))
    (helper 0 1))

(define (count-equal-end-digits a b)
  (define (counter n a b)
    (if (= (remainder a 10) (remainder b 10)) (counter (+ 1 n) (quotient a 10) (quotient b 10))
        n))
  (counter 0 a b))

(define (automorphic? n)
  (if (= (count-equal-end-digits (expt n 2) n) (countdigits n)) #t #f))

(define (prime? n)
  (if (= (count-divisors n) 2) #t #f))

(define (CubicDiff a b) (- (expt a 3) (expt b 3)))


(define (firstNCubicDiff n)
  (define (helper a b n)
    (if (= (+ a 1) n) (cons (CubicDiff a b) (helper (+ 1 a) (+ 1 b) n))
        (cons (CubicDiff a b) (helper (+ a 1) (+ 1 b) n) )))
  (helper 1 0 n))


(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op a
          (accumulate op nv (next a) b term next))))

(define the-empty-stream '())
(define (cons-stream h t) (cons h (delay t)))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)


