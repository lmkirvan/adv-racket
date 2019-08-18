#lang br

(define in (open-input-file "/home/kirvanlewis/advent/adv1.txt"))

(define adv (port->lines in))

(define (parse-one str)
  (string-ref str 0))

(define (parse-rest str)
  (string->number
   (substring str 1 (string-length str))))

(define (call-func char)
  (cond
    [(eq? char #\+) + ]
    [(eq? char #\-) - ]
    [else empty]))

(define (add-or-subtract l)
  (define (iter lst result)
    (cond
      [(empty? lst) result]
      [else (iter (rest lst)
                  ((call-func (parse-one (first lst))) result (parse-rest (first lst))))]))
  (iter l 0))


(add-or-subtract adv)
