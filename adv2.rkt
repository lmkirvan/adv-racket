#lang br
(require racket/set)

(define in (open-input-file "/home/kirvanlewis/advent/adv2.txt"))

(define adv (port->lines in))

(define (count-el el ls)
  (define (iter el ls count)
    (cond [(empty? ls) (list el count) ]
          [(eq? el (first ls)) (iter el (cdr ls) (+ count 1))]
          [else (iter el (cdr ls) count)]))
  (iter el ls 0))

(count-el #\a (string->list "aabcd"))

(define (list->table ls)
  (map (curryr count-el ls) (set->list (list->set ls))))
  



   