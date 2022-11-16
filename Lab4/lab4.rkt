#lang racket
;#Task 11.1

; Функція, яка рахує суму дільників числа
(define (f1 num)
(define (inner sum temp)
(if (< temp num)
(inner (if (= (modulo num temp) 0)
(+ sum temp)
sum)
(+ temp 1))
sum))
(inner 0 1))

(define (get-list n)
(define (inner temp)
(if (<= temp n)
(append (if (= (f1 temp) temp)
(list temp)
'())
(inner (+ 1 temp)))
'()))
(inner 1))

(display "#Task 11.1")
(newline)
(display "Список досконалих чисел, які < 10000:\n")
(define my-list (get-list 10000))
(display my-list) (newline) (newline)

;a) Визначити елементи списку, які є факторіалами чисел, та підрахувати їх кількість

(define (factorial? x)
(define (inner sum count)
(if (> sum x)
#f
(if (= sum x)
#t
(inner (* sum count) (+ 1 count)))))
(inner 1 2))

(define (task-1.a arr count)
(if (not (null? arr))
(if (factorial? (car arr))
(begin
(display (car arr))
(display " - факторіал\n")
(task-1.a (cdr arr) (+ count 1)))
(begin
(display (car arr))
(display " - НЕ факторіал\n")
(task-1.a (cdr arr) count)))
(begin
(display "Кількість факторіалів в списку: ")
(display count)
(newline))))

(task-1.a my-list 0)

; b) Видалити елементи списку, які є факторіалом числа;

(define (delete-factorial arr)
(if (not (null? arr))
(append (if (factorial? (car arr))
'()
(list (car arr)))
(delete-factorial (cdr arr)))
'()))

(define new-list (delete-factorial my-list)) ; новий список без факторіалів

(display "\nСписок без факторіалів:\n")
(display new-list) (newline)

; c) Знайти елементи, остання цифра яких дорівнює n (задається користувачем).

(define (task-1.c arr x)
(unless  (null? arr)
(begin
(when (= (modulo (car arr) 10) x)
(begin
(display (car arr))
(newline)))
(task-1.c (cdr arr) x))))

(newline)
(define x (read))
(display "\nEлементи, остання цифра яких дорівнює ")
(display x) (newline)
(task-1.c new-list x) (newline)


;#Task 11.2

(define k 4) ; кількість, людей які входять в маршрутку
(define p 2) ; кількість нових людей, які приходять на зупинку
(define m 5) ; кількість маршруток
(define n 10) ; час між двома маршрутками
(define r 3) ; кількість циклів

(define count 25) ; початкова кількість людей на зупинці
(define total 0) ; кількість перевезених людей

(display "#Task 11.2")
(newline)

(define (run)
(define (inner-2 mt)
(if (and (<= mt m)
(> count 0))
(let ((kt (if (< (+ p count) k) (+ p count) k)))
(set! count (+ (- count kt) p))
(set! total (+ kt total))
(display "Кількість людей, які поїхали на маршрутці No")
(display mt)
(display ": ")
(display kt)
(newline)
(inner-2 (+ 1 mt)))
(begin
(display "Кількість людей на зупинці в кінці циклу: ")
(display count))))
(define (inner-1 rt)
(if (and (<= rt r)
(> count 0))
(begin
(display "\n---------------Цикл ")
(display rt)
(display "---------------\n")
(inner-2 1)
(inner-1 (+ rt 1)))
(begin
(display "\n++++++++++++++++++++++++++++++++++++++\n")
(display "Сумарна кількість перевезених людей: ")
(display total)
(newline))))
(inner-1 1))
(run) 

