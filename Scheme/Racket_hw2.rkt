#lang racket
;1 задача
(define (cartesian-product-helper el xs)
  (if (null? xs) '()
      (append (list (cons el (car xs))) (cartesian-product-helper el (cdr xs)))))

(define (cartesian-product xs ys)
  (if (null? xs) '()
      (append (cartesian-product-helper (car xs) ys)
              (cartesian-product (cdr xs) ys))))

;2 задача

(define (factorize n)
  (define (iter f n lsst)
    (cond
      ((equal? n 1) '())
      ((and (> n 1) (equal? (remainder n f) 0))
       (append (list f) (iter f (/ n f) lsst)))
      (else (iter (+ 1 f) n lsst))))
  (iter 2 n '()))