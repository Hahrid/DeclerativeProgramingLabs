#lang racket

; функція для обчислення похідної
(define (deriv pol)
(if (not (null? (cdr pol)))
(append (list (* (car pol)
(- (length pol) 1))) ; формула похідної x^n = n * x^(n-1)
(deriv (cdr pol)))
'()))

; функція для виводу поліному на екран
(define (print-pol pol)
(when (not (null? pol))
(when (not (= (car pol) 0))
(begin
(display "(")
(display (car pol))
(display "*x^")
(display (- (length pol) 1))
(display ")")
(if (not (null? (cdr pol)))
(begin
(display (if (not (= (cadr pol) 0))" + " ""))
(print-pol (cdr pol)))
(newline))))))

(define polynomial (list 8 -4 6 3 -2 7))

(define d1 (deriv polynomial))

(define d2 (deriv d1))
(define d3 (deriv d2))

(display "Початкове рівняння:\n")
(print-pol polynomial)

(display "Перша похідна:\n")
(print-pol d1)

(display "Друга похідна:\n")
(print-pol d2)

(display "Третя похідна:\n")
(print-pol d3)