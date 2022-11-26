    #lang racket
(define get-vector-sum ; рахуємо суму елементів у векторі
  (lambda (vec)
    (let (
          (len (vector-length vec))
          (result  0)
          )
      (do ((index 0 (+ index 1)))
        ((= index len) result)
        (set! result (+ result (abs (vector-ref vec index))))
        )
      )
    )
  )

(define (get-elem-count vec i elem) ; дізнаємося скільки разів елемент повторюється у векторі
  (let ((sum 0))
    (do ((index 0 (+ index 1)))
      ((= index i) sum)
      (if (= elem (vector-ref vec index))
          (set! sum (+ sum 1))
          5
          )
      )
    )
  )

(define (get-elem-info vec stop i length) ; повертає список що містить (скількі разів повторюється елемент, перший індекс, значення елементу)
  (if (= i length)
      `()
      (if (= stop (vector-ref vec i))
          (get-elem-info vec stop (+ i 1) length)
          (let (
                (a (vector-ref vec i))
                (sum 0)
                )
            (do ((index i (+ index 1)))
              ((= index length)
               (if (= 0 (get-elem-count vec i a))
                   (cons (list sum i a) (get-elem-info vec stop (+ i 1) length))
                   (get-elem-info vec stop (+ i 1) length)
                   )
               )
              (if (= a (vector-ref vec index))
                  (set! sum (+ sum 1))
                  5
                  )
              )
            )
          )
      )
  )

(define (get-biggest-elem-index a b a1 a2) ; повертає індекс найбільшого елемента
  (if (< (vector-ref (list->vector a) 0) (vector-ref (list->vector b) 0))
      a2
      a1
      )
  )

(define (get-most-repeated-elem vec) ; повертає елемент який більше всього повторюється
  (let (
        (res (list->vector (get-elem-info vec (+(get-vector-sum vec) 1) 0 (vector-length vec))))
        (a 0)
        )
    (display res)
    (newline)
    (do ((index 0 (+ index 1)))
      ((= index  (vector-length res) ) (vector-ref res a))
      (set! a (get-biggest-elem-index (vector-ref res a) (vector-ref res index) a index))
      )
    )
  )

(let ((most-repeated-elem (get-most-repeated-elem '#(1 9 9 8 1 9 9 9 8 11 2))))
  (display "Number ")
  (display (vector-ref (list->vector most-repeated-elem) 2))
  (display ", encounter times ")
  (display (vector-ref (list->vector most-repeated-elem) 0))
  (display ", first index is ")
  (display (vector-ref (list->vector most-repeated-elem) 1))
  )