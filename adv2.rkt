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

(define (list->table ls)
  (map (curryr count-el ls) (set->list (list->set ls))))

(define (count-matches table n)
  (let ([counts (map (curryr list-ref 1) table)])
    (length (filter (curryr eq? n) counts))))

(define adv-tables (map list->table (map string->list adv)))

(define twos (map (curryr count-matches 2) adv-tables))
(define threes (map (curryr count-matches 3) adv-tables))

(define twos-lgl (map (lambda (n)
       (if (positive? n) 1 0)) twos))

(define threes-lgl (map (lambda (n)
       (if (positive? n) 1 0)) threes))

(* (apply + twos-lgl) (apply + threes-lgl))



