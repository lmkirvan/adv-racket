#lang br
(require racket/set)

(define in (open-input-file "/home/kirvanlewis/advent/adv2.txt"))

(define adv (port->lines in))

(define (string->set str)
  (list->set (string->list str)))

(define (number-repeated str)
  (let ([setl (set-count (string->set str))]
        [strl (string-length str)])
    (- strl setl)))

(define (remove-repeated str)
  (let ([sl (string->list str)])
    (cond [(empty? sl) empty]
          [(member (first sl) (rest sl)) (remove-repeated (list->string (rest sl)))]
          [else (cons (first sl) (remove-repeated (list->string (rest sl))))]
  )))

(define (vector-sub list1 list2)
  (cond [(or (empty? list1) (empty? list2)) empty]
        [else (cons
               (- (first list1) (first list2))
               (vector-sub (rest list1) (rest list2)))]
        ))

(define (count-reps str)
  (let* ([nr (number-repeated str)]
         [lr (vector-sub (length remove-repeated) nr)])
    (cond [(eq? (modulo lr nr) (modulo lr nr)) (eval-equal nr lr)]
          
    )))



(define (eval-equal nr lr)
  (let ([res-two (/ lr 2)]
        [res-three (/ lr 3)]
        [res-five (/ lr 5])
    (cond [(eq? res-two nr)]
    ))
  
(define (even n)
  (cond [(eq? (modulo n 2) 0) #t]
        [else #f]))

(define (divisible-by n m)
  (cond [(eq? (modulo n m) 0) #t]
        [else #f]))

