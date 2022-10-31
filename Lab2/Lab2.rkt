#lang racket


(define factorial 
  (lambda (n)
    (if (= n 0) 
        1
        (* n (factorial (- n 1))))))


(define (cosx x precision)
  (ex_step x 0 0 precision)
)


(define (ex_step x n sum precision)
  (let(
       (element (*(/ (expt -1 n) (factorial (+ (* 2 n) 1)))(expt x (+ (* 2 n) 1))))
       )
    (if (>= (abs element) precision)
        (ex_step x (+ n 1) (+ sum element) precision)
        sum
        )
    )
  )

(define (first_function x precision)
  (- (expt (cosx (* -1 x) precision) 2) (cosx x precision))
  )

(define (second_function x precision)
  (+ (cosx (expt x 3) precision) (cosx (* 2 x) precision))
  )

(define (quest_1_func x precision)
  (if (and (>= x -1) (<= x 1))  
      (first_function x precision)
      (if (and (> x 1) (<= x 2)) (second_function x precision) (0))
      )
  
  )


        

          

(provide perfect-numbers)     
(define (sum-of-proper-divisors n [i (- n 1)])
  (cond
    [(= i 0) 0]
    [(= i 1) 1]
    [(= (modulo n i) 0)
     (+ i (sum-of-proper-divisors n (- i 1)))]
    [else (sum-of-proper-divisors n (- i 1))]))

        
(define (perfect-number? n)
  (= (sum-of-proper-divisors n) n))

(define (perfect-numbers n)
  (if (= 0 n)
    '()
    (if (perfect-number? n)
      (sort (cons n (perfect-numbers (- n 1))) <)
      (perfect-numbers (- n 1)))))


(define (perfect-less n)
  (cond
    ((= 0 n)
      (printf "END"))
    ((perfect-number? n)
      (printf "~a " n)
      (perfect-less (- n 1)))
  (else
   (perfect-less (- n 1)))))
