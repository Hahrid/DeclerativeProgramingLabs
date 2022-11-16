#lang racket

(define (function x) (/(cos x)(sqrt x)))


;;---------------------------------------------------------------

;;Simpson Method

(define (identity x) x)
(define (inc x) (+ x 1))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(integral function 0.1 2 10)



;;---------------------------------------------------------------

;;Trapezoid method


(define (trapezoid func p1 p2 n)

(define (calc-h a b n)
  (/ (- b a) n))

  (define (trapez fun p1 p2 h n sum aux)
    (cond ((zero? n)
           (* (+ sum (fun p1) (fun p2)) (/ h 2)))
          (else
           (trapez fun p1 p2 h (- n 1) (+ sum (* (fun aux) 2)) (+ aux h)))))
  


(trapez func p1 p2 (calc-h p1 p2 n) (- n 1) 0 (+ p1 (calc-h p1 p2 n))))


(trapezoid function 0.1 2 10)


;;---------------------------------------------------------------
;;###############################################################
;;---------------------------------------------------------------

(define (needle-func x)
  (- (expt x 2)(cos x))
  )





;Метод простої ітерації
(define (prI a b)
  (define x0 x)
  (cond
    [(<= a b)
      (begin
        (set! i (+ i 1))
        (set! x (sqrt (cos x0)))
        (if (or (< (abs (- x x0)) toch) (> i im))
            (begin
              (display "a= ")
              (display a)
              (newline)
              (display "x= ")
              (display x)
              (newline)
              (display (- (* x x) (cos x)))
              (newline)
              (set! x (+ a 0.5))
              (prI (+ a 0.5) b))
            (prI a b)))]
    ))
;Метод перебору
(define (per a b x)
  (cond
    [(<= a b)
      (begin
        (if (= (- (* x x) (cos x)) 0)
            (begin
              (set! x (+ x toch))
              (per a b x))
            (begin
              (display "a= ")
              (display a)
              (newline)
              (display "x= ")
              (display x)
              (newline)
              (display (- (* x x) (cos x)))
              (newline)
              (set! x (+ a 0.5))
              (per (+ a 0.5) b x))))]
      ))

(define i 0)
(define im 50)
(define toch 0.001)
(define delta (* toch 2))
(define a (read))
(define b (read))
(define x a)

(display "Метод простої ітерації")
(newline)
(prI a b)
(newline)
(display "Метод перебору")
(newline)
(per a b a)
