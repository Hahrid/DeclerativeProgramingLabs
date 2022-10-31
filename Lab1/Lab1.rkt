#lang racket

(define (pascal x y)
  (if (or (zero? y) (= x y))
      1
      (+ (pascal (sub1 x) y)
         (pascal (sub1 x) (sub1 y)))))

(define (pascal-triangle m n)
  (if (< m n)
      (for/list ([x (in-range m n)])
        (for/list ([y (in-range 0 (add1 x))])
          (pascal x y)))
      (error "Error: m > n")))


(define l '( 1 2 3 4 5 6 7 8 9 0))

(define (odds lst)
  (filter even? lst))



(define (remove-even y)
  (cond
    ((null? y) '())
    ((pair? (car y))
     (cons (remove-even (car y)) (remove-even (cdr y))))
    ((or (not(= (remainder (car y) 2) 0))(zero? (car y))) 
     (cons (car y) (remove-even (cdr y))))
    (else (remove-even (cdr y)))))




(define x (map string->number (string-split (read-line))))


(define (Recursion)
  (cond
    ((null? x) "")
    ((not(= (remainder (car x) 2) 0))
    (printf "~a " (car x))
    (set! x (remove (car x) x))
    (Recursion))
    (else
     (set! x (remove (car x) x))
    (Recursion)))
)

(Recursion)
