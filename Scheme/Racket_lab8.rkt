#lang racket
;Задача 1. Да се дефинира функция (sum-numbers a b), приемаща два аргумента, която
;намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред.
(define (check? a)
  (cond
    ((and (> a -1) (< a 10))     #t)
    ((<= (remainder a 10) (remainder (quotient a 10) 10)) (check? (quotient a 10)))
    (else       #f )))

(define (sum-numbers a b)
  (define (helper res a b)
    (cond
      ((> a b) res)
      ((check? a)   (helper (+ res a) (+ a 1) b))
      ((helper res (+ a 1) b))))
  (helper 0 a b))

;Задача 2. Да се дефинира функция (num-bigger-elements lst), която за даден списък от
;числа lst връща като резултат списък с елементи от вида (lsti ni), където lsti е i-тият
;елемент на lst, а ni е броят на елементите на lst, които са по-големи от lsti.

(define (getElAtPos pos lst)
  (define (helper iter lst)
    (cond
      ((null? lst) '())
      ((= pos iter) (car lst))
      (else    (helper (+ 1 iter) (cdr lst))   )))
  (helper 0 lst))

;пресмята броя по-големи числа на даден елемент от списъка
(define (countLargerNumbers lst pos)
  (define currEl (getElAtPos pos lst))
  (define (helper counter lst)
     (cond
      ((null? lst)   counter )
      ((< currEl (car lst)) (helper (+ 1 counter) (cdr lst)))
      (else (helper counter (cdr lst)))))
  (helper 0 lst))

(define (getThePosOfEl el lst)
  (define (iter it el lst)
    (cond
      ((null? lst) '())
      ((equal? el (car lst)) it)
      (else (iter (+ 1 it) el (cdr lst)))))
  (iter 0 el lst))

(define (num-bigger-elements lst)
   (map (lambda (x) (cons x (countLargerNumbers lst (getThePosOfEl x lst)))) lst))


;Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на
;естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k

;Да направим функция, която проверява дали сумата на цифрите на число е кратна на к

;Да направим първо функция, която намира сумата на цифрите
(define (sumDigits n)
  (define (helper n res)
    (if (equal? n 0) res
        (+  res (remainder n 10) (helper (quotient n 10) res))))
  (helper n 0))

(define (checkMultipleRoot? numb k)
  (if (equal? (remainder numb k) 0) #t #f))

(define (accumulate op a b term next acc)
  (if (> a b) acc
      (op (term a)
          (accumulate op (next a) b term next acc))))

(define (sum-sum-digit a b k)
  (accumulate + a b
              (lambda (x) (if (checkMultipleRoot? x k) x 0))
              (lambda (x) (+ 1 x))
              0))
;Задача 8. Да се дефинира функция (set-union xs ys), която връща обединението на
;множествата от числа xs и ys, представени като списъци, наредени във възходящ ред.
;Елементите на резултантното множество също трябва да са наредени във възходящ ред.
;Примери:
;(set-union '(1 3 5 7) '(5 7 13)) → '(1 3 5 7 13)
;(set-union '(5 7 13) '(1 3 5 7)) → '(1 3 5 7 13)

(define (set-union xs ys)
  (cond
     ((null? xs) ys)
     ((null? ys) xs)
     ((< (car xs) (car ys)) (cons (car xs) (set-union (cdr xs) ys)))
     ((< (car ys) (car xs)) (cons (car ys) (set-union xs (cdr ys))))
     ((equal? (car xs) (car ys)) (cons (car xs) (set-union (cdr xs) (cdr ys))))))


;Задача 4. Да се дефинира функция (repeater str), която получава като аргумент символен
;низ и връща анонимна функция на два аргумента - count и glue (число и низ). Оценката на
;обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне
;на низа str, при което между всеки две съседни повторения на str стои низът glue.
;примери:
;> ((repeater "I love Racket") 3 " ")
;"I love Racket I love Racket I love Racket"
;> ((repeater "Quack") 5 "!")
;"Quack!Quack!Quack!Quack!Quack"
;Помощна информация. За да съедините няколко низа, може да използвате вградената
;функцията string-append:
;> (string-append "I" "Love" "Racket")
;"ILoveRacket"
;Функцията string-append приема произволен брой агрументи и връща низ, който
;представлява тяхната конкатенация.

(define (repeater str)
  (lambda (count glue)
    (if (<= count 1) str
        (string-append str glue
                       ((repeater str) (- count 1) glue)))))

; Малко рандом полезни нещица
(define (atom? a) (not (pair? a)))

(define (count-atoms lst)
  (cond [(null? lst) 0]
        [(atom? (car lst)) (+ 1 (count-atoms (cdr lst)))]
        [else (+ (count-atoms (car lst)) (count-atoms (cdr lst)))]))

(define (reverse lst)
  (if (null? lst) '()
      (append (reverse (cdr lst)) (list (car lst)))))

(define (deep-reverse lst)
  (if (atom? lst) lst
        (append (deep-reverse (cdr lst))
                        (list (deep-reverse (car lst))))))

; Задача 6. Да се дефинира функция (max-ordered-sublist lst), която намира найдългия възходящо сортиран подсписък на списъка от числа lst.
;Пример:
;(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) → '(2 4 6 8)
;ЗАДАЧАТА Я ПРЕПИСАХ НАГОТОВО, ВЪРНИ СЕ КЪМ НЕЯ ПАК!

(define (max-ordered-prefix1 lst res)
  (cond
    ((equal? (length lst) 1) (cons (car lst) res))
    ((null? lst) '())
    ((< (car lst) (cadr lst)) (cons (car lst) (max-ordered-prefix1  (cdr lst) res)))
    ((> (car lst) (cadr lst)) (max-ordered-prefix1  (cdr lst) res))
    ((equal? (car lst) (cadr lst)) (cons (car lst) (max-ordered-prefix1 (cdr lst) res)))))

(define (max-ordered-prefix xs)
  (cond
    ((null? xs) '())
    ((or (null? (cdr xs)) (>= (car xs) (cadr xs))) (list (car xs)))
    (else (cons (car xs) (max-ordered-prefix (cdr xs))))))

(define (max-ordered-sublist xs)
  (define (helper xs max-sublist)
    (define cur-ord-pref (max-ordered-prefix xs))
    (cond
      [(null? xs) max-sublist]
      [(> (length cur-ord-pref) (length max-sublist))
       (helper (drop xs (length cur-ord-pref)) cur-ord-pref)]
      [else   (helper (drop xs (length cur-ord-pref)) max-sublist)]))
  (helper xs '()))

;Задача 6. Функция (flatten xss), която приема списък от списъци (които също могат да са от списъци,
;т.е. имаме произволно ниво на вложение) и връша списък само от елементи, т.е. списък без вложени списъци
;Пример: (flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))) -> '(1 2 3 4 5 6 7 8 9 10 11 12)
    
(define (flatten xs)
  (cond [(null? xs) '()]
        [(list? (car xs)) (append (flatten (car xs)) (flatten (cdr xs)))]
        [else   (cons (car xs) (flatten (cdr xs)))]))

;Асоциативен списък - списък от точкови двойки (<ключ> . <асоциация>) 
;Задача 7. Функция (assoc key a-list), която търси key в асоциативния списък a-list и връша първия му елемент с ключ равен на key.
;Задача 8. Функция (replace lst dict), която връща като резултат списък, получен от lst, в който елементите са заменени с асоциацията им в dict.

(define (assoc key a-list)
  (cond
    [(null? a-list) (display "no such key")]
    [(equal? (caar a-list) key) (cadr (car a-list))]
    [else  (assoc key (cdr a-list))]))

;Задача 7. Да се дефинира функция (where list-elements list-predicates), която
;връща списък от всички елементи на list-elements, за които са изпълнени всички
;предикати в list-predicates.
;Примери:;
;(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) →
;(6 8 10) (списък от всички елементи на дадения, които са четни числа, по-големи от 5)
;(where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) → () (в списъка
;няма четни числа, по-големи от 5)

(define (where-helper el list-predicates)
  (cond
    ((null? list-predicates) #t)
    (((car list-predicates) el) (where-helper el (cdr list-predicates)))
    (else #f)))

(define (where list-el list-pred)
  (define (result lst list-el list-pred)
    (cond
      ((null? list-el) lst)
      ((where-helper (car list-el) list-pred) (append (list (car list-el)) (result lst (cdr list-el) list-pred)))
      (else  (result lst (cdr list-el) list-pred))))
  (result '() list-el list-pred))


;Задача 6. Да се дефинира функция (max-ordered-sublist lst), която намира найдългия възходящо сортиран подсписък на списъка от числа lst.

(define (max-ord-pref lst)
  (cond
    ((null? lst) '())
    ((or (null? (cdr lst)) (>= (car lst) (cadr lst))) (list (car lst)))
    (else (cons (car lst) (max-ord-pref (cdr lst))))))

(define (max-ord-sub lst)
  (define (helper lst max-sublist)
    (define curr-ord-pref (max-ord-pref lst))
    (cond
      ((null? lst) max-sublist)
      ((> (length curr-ord-pref) (length max-sublist))
       (helper (drop lst (length curr-ord-pref)) curr-ord-pref))
      (else (helper (drop lst (length curr-ord-pref)) max-sublist))))
  (helper lst '()))

;Задача 3. Ако f и g са числови функции и n е естествено число, да се дефинира функция от повисок ред (switchsum f g n), която връща като резултат функция, чиято стойност в дадена
;точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).
;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2) → 3

(define (switchsum f g n)
  (define (helper count prev result)
    (cond [(>= count n) result]
          [(= (remainder count 2) 0)
           (helper (+ 1 count) (g prev) (+ result prev))]
          [else (helper (+ count 1) (f prev) (+ result prev))]))
  (λ (x) (helper 0 (f x) 0)))

;Задача 1. "Метрика" наричаме функция, която приема като параметър списък от числа и връща число като резултат.
;Да се напише функция best-metric?,която приема като параметри списък от метрики ms и списък от списъци от числа
;xss и проверява дали има метрика в ms, която дава по-големи стойности от всички други метрики от ms над всеки от елементите на xs.

;(best-metric? (list sum prod) `((0 1 2) (3 -4 5) (1337 0)))  ; -> #t
;(best-metric? (list car sum) `((100 -100) (29 1) (42)))      ; -> #f

(define (prod xs) (apply * xs))
(define (sum xs) (apply + xs))

(define (best-metric? ms xss)
  (define applied-metrics (map (λ (m) (map m xss)) ms))
  (define (helper aps)
    (cond [(null? aps) #f]
          [(andmap (λ (m) (andmap >= (car aps) m)) applied-metrics) #t]
          [else (helper (cdr aps))]))
  (helper applied-metrics))

(define (f ms xss)
  (map (λ (m) (map m xss)) ms))

(f (list car prod sum) '((-100 1) (29 1) (42)))

(andmap (λ (t) (andmap >= '(-99 30 42) t))
        '((-100 29 42) (-100 29 42) (-99 30 42)))

;Задача 2. "Ниво на влагане" на атом в дълбок списък наричаме броя пъти, който трябва да се приложи операцията car за достигане до атома.
;Да се реализира функция deep-delete, която в даден дълбок списък изтрива всички атоми, които са по-малки от нивото им на влагане.
;Пример: (deep-delete `(1 (2 (2 4) 1) 0 (3 (1)))) ; -> (1 (2 (4)) (3 ())))

(define (deep-delete ls)
  (define (helper level lscur)
    (cond [(null? lscur) '()]
          [(list? (car lscur)) (cons (helper (+ 1 level) (car lscur))
                                     (helper level (cdr lscur)))]
          [(< (car lscur) level) (helper level (cdr lscur))]
          [else (cons (car lscur) (helper level (cdr lscur)))]))
  (helper 1 ls))

;Задача 3. Да се дефинира предикат (hasMatchingLengths l1 l2). l1 и l2 са непразни списъци от списъци от числа.
;Ако l1 = '(a1 a2 … ak), а l2 = '(b1 b2 … bk), предикатът да връща истина, когато разликите в дължините на всички двойки съответните списъци ai и bi са еднакви.

(define (hasMatchingLengths ls1 ls2)
  (cond
    ((null? (cdr ls1)) #t)
    [(equal? (- (length (car ls1)) (length (car ls2)))
             (- (length (cadr ls1)) (length (cadr ls2))))
     (hasMatchingLengths (cdr ls1) (cdr ls2))]
    [else #f]))



   