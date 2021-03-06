#lang racket
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



;Още функции за работа със списъци
;1) (take xs n) - взима първите n елемента на списък
;2) (drop xs n) - премахва първите n елемента на списък
;3) (zip xs ys) - връща списък от двойки (Ai,Bi), където Ai е i-тия елемент на xs, Bi е i-тия елемент на ys

;Функции от по-висок ред за работа със списъци:
;1) (any? pred? xs) - проверява дали поне един елемент на списъка удовлетворява даден предикат
;2) (all? pred? x) - проверява дали всеки елемент на списъка удовлетворява даден предикат
;3) (map f xs) - прилага f към всеки елемент на xs
;4) (filter p xs) - премахва (филтрира) елементите, за които предикатът p е грешен
;5) (apply f xs) - връща оценката на f със аргументи - елементите на списъка 
;6) foldl, foldr - обхождане на списък с последователно прилагане на f
;7) (zipWith f xs ys) - връща списък с елементи функцията f приложена към елемент на xs и елемент на ys

;Задача 1. Функция (removeDuplicates xs), която премахва всички повторни срещания на елементи от списъка.
;Задача 2. Функция (sublistBetween start end xs), която взима подсписъка на xs между позициите start и end.
;Задача 3. Функция (countOcccurrences subxs xs), която връща броя срещания на subxs в xs.
;Задача 4. Функция (ordered? xs pred), която проверява дали списък е сортиран възходящо/низходящо според подадена функция за сравнение.
;Задача 5. Функция (maxOrderedSublist xs), която връща най-дългия възходящо сортиран подсписък от списъка xs.
;Задача 6. Функция (flatten xss), която приема списък от списъци (които също могат да са от списъци, т.е. имаме произволно ниво на вложение) и връша списък само от елементи, т.е. списък без вложени списъци
;Пример: (flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))) -> '(1 2 3 4 5 6 7 8 9 10 11 12)
;Асоциативен списък - списък от точкови двойки (<ключ> . <асоциация>) 
;Задачи:
;Задача 7. Функция (assoc key a-list), която търси key в асоциативния списък a-list и връша първия му елемент с ключ равен на key.
;Задача 8. Функция (replace lst dict), която връща като резултат списък, получен от lst, в който елементите са заменени с асоциацията им в dict.




;1) 
(define (my-take xs n)
  (define (helper xs n i res)
    (if (= i n) res
        (cons (car xs) (helper (cdr xs) n (+ i 1) res))))
  (helper xs n 0 '()))

;2)
(define (my-drop xs n)
  (define (helper xs n i)
    (if (= i n) xs
        (helper (cdr xs) n (+ i 1))))
  (helper xs n 0))

;3)
(define (my-zip xs ys)
  (define (helper xs ys res)
    (if (and (null? xs) (null? ys)) res
        (cons (car xs)(cons (car ys) (helper (cdr xs) (cdr ys) res)))))
  (helper xs ys '()))

;4)
(define (any? pred? xs)
  (cond
    ( (null? xs) #f)
    ( (pred? (car xs))    #t)
    ( else       (any? pred? (cdr xs)))))

(define (even? a)
  (if (= (remainder a 2) 0) #t #f))

;5) 
(define (all? pred? xs)
  (cond
    ((null? xs)      #t)
    ((pred? (car xs)) (all? pred? (cdr xs)))
    (else #f)))

;6)
(define (map f xs)
  (define (helper f xs res)
    (if (null? xs) res
        (cons (f (car xs)) (helper f (cdr xs) res))))
  (helper f xs '()))

(define f (λ (x) (+ x 2)))

;7)
(define (my-filter p? xs)
  (define (helper p? xs res)
    (cond
      ((null? xs) res)
      ((p? (car xs)) (cons (car xs) (helper p? (cdr xs) res)))
      (else (helper p? (cdr xs) res))))
  (helper p? xs '()  ))

;8)
(define (apply op xs)
  (define (helper op xs res)
    (if (null? xs) res
        (op (car xs) (helper op (cdr xs) res))))
  (helper op xs 0))

; До тук нещата ще ги ползваме наготово

;\/Задача 1. Функция (removeDuplicates xs), която премахва всички повторни срещания на елементи от списъка.
;помощна (remove el xs) - премахва всички срещания на el в xs

(define (removeEl el xs)
  (cond
    ((null? xs) '())
    ((= (car xs) el) (removeEl el (cdr xs)))
    (else (cons (car xs) (removeEl el (cdr xs))))))

; долу е мн яко решение само дето не се сетих сам за него
(define (removeDuplicates xs)
  (if (null? xs) '()
      (cons (car xs) (removeDuplicates (removeEl (car xs) (cdr xs))))))

;\/Задача 2. Функция (sublistBetween start end xs), която взима подсписъка на xs между позициите start и end.

(define (my-sublistBetween start end xs)
  (define (helper start end xs iter)
    (cond
      ((= (length xs) 1) xs)
      ((= iter start) (take xs (+ (- end start) 1)))
      (else (helper start end (cdr xs) (+ iter 1)))))
  (helper start end xs 0))

(define (sublistBetween start end xs)
  (take (drop xs start) (+ 1 (- end start))))

;\/Задача 3. Функция (countOcccurrences subxs xs), която връща броя срещания на subxs в xs.
(define (prefix? subxs xs)
  (cond
    ((null? subxs)    #t)
    ((= (car subxs) (car xs)) (prefix? (cdr subxs) (cdr xs)))
    (else #f)))

(define (countOccurrences subxs xs)
  (define (helper counter curr)
    (cond
      ((null? curr)    counter)
      ((prefix? subxs curr) (helper (+ counter 1) (cdr curr) ))
      (else (helper counter (cdr curr)))))
  (helper 0 xs))

;\/Задача 4. Функция (ordered? xs pred), която проверява дали списък е сортиран възходящо/низходящо според подадена функция за сравнение.
; мое разсъждение за итерирането тук - щом е останал само един елемент значи всички проверки са минали тоест е подреден списъкът
(define (ordered? xs pred)
  (cond
    ((= (length xs) 1) #t)
    ((pred (car xs) (car (cdr xs))) (ordered? (cdr xs) pred))
    (else #f)))

;Задача 5. Функция (maxOrderedSublist xs), която връща най-дългия възходящо сортиран подсписък от списъка xs.
;идея правим списък от всички подсписъци
;22.11.2020 Изобщо изключих, че мога да използвам предишните задачи !!!

;първо ще направим max-ordered-prefix, което дава само максималния сортиран префикс префикс
(define (max-ordered-prefix xs)
  (cond
    ((null? xs)   '())
    ((or (>= (car xs) (cadr xs))
         (null? (cdr xs))) (list (car xs)))
    (else       (cons (car xs) (max-ordered-prefix (cdr xs))))))


                                    
      
      
      
  

;Много готина задачка, даваща ни всички подсписъци на списък ЗАПОМНИ !!!
(define (subsets xs)
  (if (null? xs) (list null)
      (let ((rest (subsets (cdr xs))))
        (append rest
                (map (λ (x) (cons (car xs) x)) rest)))))

(define (isSorted? xs)
  (cond
    ((null? xs) #t)
    ((= (length xs) 1) #t)
    ((< (car xs) (cadr xs)) (isSorted? (cdr xs)))
    (else #f)))

;Задача 6. Функция (flatten xss), която приема списък от списъци (които също могат да са от списъци, т.е. имаме произволно ниво на вложение) и връша списък само от елементи, т.е. списък без вложени списъци
;Пример: (flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))) -> '(1 2 3 4 5 6 7 8 9 10 11 12)

;Асоциативен списък - списък от точкови двойки (<ключ> . <асоциация>) 
;Задачи:

;Задача 7. Функция (assoc key a-list), която търси key в асоциативния списък a-list и връша първия му елемент с ключ равен на key.

;Задача 8. Функция (replace lst dict), която връща като резултат списък, получен от lst, в който елементите са заменени с асоциацията им в dict.



