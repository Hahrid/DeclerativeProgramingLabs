; Task 6.2
#lang r5rs

(define (create-stack)
  `()
  )

(define (add-to-stack stack elems)
  (append (list elems) stack)
  )

(define (stack-cdr stack)
  (cdr stack)
  )

(define (stack-car stack)
  (car stack)
  )

(define (create-queue) ; створюємо чергу
  (define lists
    (cons '() '())
    )
  (cons lists lists)
  )

(define (queue-has-length queue) ; перевіряємо чергу на довжину
  (and (eq? (get-first-item queue) (get-last-item queue)) (eq? (car (get-first-item queue)) '()))
  )

(define (get-first-item queue) ; дістаємо перший елемент з черги
  (car queue)
  )

(define (get-last-item queue) ; дістаємо останній елемент с черги
  (cdr queue)
  )

(define (add-to-queue queue element) ; додаємо новий елемент у чергу
  (define last-item
    (cons element '())
    )
  (if (queue-has-length queue)
      (begin (set-car! queue last-item)
             (set-cdr! queue last-item)
             )
      (begin
        (set-cdr! (get-last-item queue) last-item)
        (set-cdr! queue last-item)
        )
      )
  )

(define (remove-from-queue queue) ; видалення елементу з черги
  (define x 0)
  (if (queue-has-length queue)
      'Queue-is-empty
      (if (and (eq? (get-first-item queue) (get-last-item queue)) (eq? '() (cdr (get-first-item queue))))
          (begin
            (set! x (car (get-first-item queue)))
            (set-car! (get-first-item queue) '())
            x
            )
          (begin
            (set! x (car (get-first-item queue)))
            (set-car! queue (cdr (get-first-item queue)))
            x
            )
          )
      )
  )

; Створення двох черг
(define (copy queues)
  (let
      (
       (f (create-queue))
       (s (create-queue))
       (x 1)
       )

    (do ((index 0 (+ index 1)))
      ((queue-has-length queues) (list f s))
      (begin
        (set! x (remove-from-queue queues))
        (add-to-queue f x)
        (add-to-queue s x)
        )
      )
    )
  )

(define (compare-queues first-queue second-queue) ; перевіряємо чи однакові черги
  (if (or (queue-has-length first-queue) (queue-has-length second-queue))
      (and (queue-has-length first-queue) (queue-has-length second-queue))
      (let
          (
           (f (remove-from-queue first-queue))
           (s (remove-from-queue second-queue))
           )
        (if (= f s)
            (compare-queues first-queue second-queue)
            #f
            )
        )
      )
  )

(define (queue-to-stack queue) ; трансформуємо чергу у стек
  (let ((r (create-stack)))
    (do ((index 0 (+ index 1)))
      ((queue-has-length queue) r)
      (set! r (add-to-stack r (remove-from-queue queue)))
      )
    )
  )

(define (stack-to-queue stack) ; трансформуємо стек у чергу
  (let
      (
       (queue (create-queue))
       (stack2 stack)
       )
    (do ((index 0 (+ index 1)))
      ((null? stack2) queue)
      (begin
        (add-to-queue queue (stack-car stack2))
        (set! stack2 (stack-cdr stack2))
        )
      )
    )
  )

(define (concatenate f s) ; створення з двох черг одної
  (do ((index 0 (+ index 1)))
    ((queue-has-length s) f)
    (add-to-queue f (remove-from-queue s))
    )
  )


(define (only-negative a) ; виборка тільки негативних чисел
  (let
      (
       (result (create-queue))
       (t 1)
       )
    (do ((index 0 (+ index 1)))
      ((queue-has-length a) result)
      (begin (set! t (remove-from-queue a))
             (if (< t 0)
                 (add-to-queue result t)
                 0
                 )
             )
      )
    )
  )



(let
    (
     (first-queue (create-queue))
     (second-queue (create-queue))
     )

  (add-to-queue first-queue 1)
  (add-to-queue first-queue 2)
  (add-to-queue first-queue 3)
  (add-to-queue first-queue 4)

  (add-to-queue second-queue 1)
  (add-to-queue second-queue 2)
  (add-to-queue second-queue 3)
  (add-to-queue second-queue 4)


  (display "First queue: ")
  (display first-queue)
  (newline)
  (display "Second queue: ")
  (display second-queue)
  (newline)


  (let
      (
       (f (list->vector (copy first-queue)))
       (s (list->vector (copy second-queue)))
       )
    (if (compare-queues (vector-ref f 0) (vector-ref s 0))
        (display (stack-to-queue (queue-to-stack (vector-ref f 1))))
        (display (concatenate (only-negative (vector-ref f 1)) (only-negative (vector-ref s 1))))
        )
    )
  )