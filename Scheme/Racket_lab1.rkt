#lang racket

(define (myfact n)
  (if (= n 1) 1
      (* n (myfact (- n 1)))))

(define (myfib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
    (else (+ (myfib (- n 2)) (myfib (- n 1))))))

(define (fib-iter n)
  (helper 1 1 1 n))

(define (helper prev curr i n)
  (if (>= i n)
      curr
      (helper curr (+ curr prev) (+ i 1) n)))

(define (mygcd a b)
  (cond
    [(= a 0) b]
    [(= b 0) a]
    [(= a b) a]
    [(> a b) (mygcd (- a b) b)]
    [else (mygcd a (- b a))]))

; finding max divisor of number x
;намира първия делител като пробва от x-1 и намалява 
;така първия намерен делител ще е най-големият

(define (mymaxdivisor x)
  (helper1 (- x 1) x))

(define (helper1 d x)
  (if (= (remainder x d) 0)
      d
      (helper1 (- d 1) x)))

(define (accumulate1 op nv a b)
  (if (> a b)
      nv
      (op a
          (accumulate1 op nv (+ 1 a) b))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (sumodds a b)
  (if (= (remainder a 2) 0)
      (accumulate + 0 (+ 1 a) b (lambda (i) i) (lambda (i) (+ i 2)))
      (accumulate + 0 a b (lambda (i) i) (lambda (i) (+ i 2)))))


(define (count-divisors n)
  (accumulate + 0 1 n
              (lambda (i) (if (equal? (remainder n i) 0) 1 0))
              (lambda (i) (+ 1 i))))

(define (prime? n)
  (if (= (count-divisors n) 2) #t #f))

(define (countdigits n)
  (define (helper n counter)
    (if (equal? n 0) counter
        (helper (quotient n 10) (+ 1 counter))))
    (helper n 0))

;(define (checkPalindrome? n)
;  (if
;    (equal? (remainder (countdigits n) 2) 0)
;    (checkPalindromeEven n)
;    (checkPalindromeOdd n)))


(define (checkPalindromeEven n)
  (define (helper n countsteps)
    (cond
     [(equal? n 0) countsteps] 
     [(equal? (remainder n 10) (quotient n (expt 10 (countdigits (- n 1))))) (+ 1 countsteps)
                                                                             (helper (- n (+ (remainder n 10) (* (quotient n (countdigits n)) (expt 10 (- (countdigits n) 1))))) (+ 1 countsteps))]))
    (helper n 0))


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


