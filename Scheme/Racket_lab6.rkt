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



;Задача 1. Да се дефинира функция, която намира дължината на списък.

;Задача 2. Да се дефинира функция, която проверява дали даден елемент се съдържа в списък.

;Задача 3. Да се дефинира функция, която добавя елемент на зададена позиция в списък.

;Задача 4. Да се дефинира функция, която намира най-малкия елемент на списък.

;Задача 5. Да се дефинира функция, която изтрива първото срещане на даден елемент в списък.

;Задача 6. Да се дефинира функция, която изтрива всички срещания на даден елемент на списък.

;Задача 7. Да се напише функция, която конкатенира два списъка.

;Задача 8. Да се напише функция, която обръща даден списък.





;1 задача
(define (list-length lst)
  (define (counter n lst)
    (if (null? lst) n
        (counter (+ n 1) (cdr lst))))
  (counter 0 lst))

;2 задача
(define (list-el? lst el)
  (define (iter i el lst)
    (cond
      [(null? lst) #f]
      [(= i el) #t]
      [else (iter (car lst) el (cdr lst))]))
  (iter (car lst) el lst))

;3 задача
(define (addElement el pos lst)
  (define (iter counter lst)
    (cond
      ((null? lst) (display "Error position"))
      ((< counter pos) (cons (car lst) (iter (+ counter 1) (cdr lst))  ))
      ((= counter pos) (cons (car lst) (cons el (cdr lst))))))
  (iter 0 lst))

(define (member? x lst)
  (cond
    [(null? lst) #f]
    ((= x (car lst)) #t)
    (else (member? x (cdr lst)))))

(define mylist '(1 2 3 4 -5 6 7))

;4 задача
(define (smallestEl lst)
  (define (iter lst tmp)
    (cond
      ((null? lst) tmp)
      ((>= (car lst) tmp) (iter (cdr lst) tmp))
      ((< (car lst) tmp) (iter (cdr lst) (list-ref lst 0)))))
  (iter lst (car lst)))

;5 задача
(define (myRemove el lst)
  (if (= el (car lst)) (cdr lst)
      (cons (car lst) (myRemove el (cdr lst)))))
  
;6 задача
(define (myRemove2 el lst)
   (cond
    ((null? lst) '())
    ((= (car lst) el) (myRemove2 el (cdr lst)))
    (else (cons (car lst) (myRemove2 el (cdr lst))))))

;7 задача
(define (myconcat lst1 lst2)
  (if (null? lst1) lst2
      (cons (car lst1) (myconcat (cdr lst1) lst2))))
  
;8 задача
(define (myReverse lst)
  (define (helper lst rev-lst)
    (if (null? lst) rev-lst
        (helper (cdr lst) (cons (car lst) rev-lst))))
  (helper lst '()))





