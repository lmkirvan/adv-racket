#lang racket

;lower triangle matrix data representation

(define in (open-input-file "/home/kirvanlewis/advent/adv2.txt"))
(define adv (port->lines in))

(define (lower-triangle ls)
  (cond [(empty? ls) empty]
        [else (cons (cons (list (first ls)) (cdr ls))
                    (lower-triangle (cdr ls)))]))

(define (simple-edit-distance str1 str2)
  (define (iter ls1 ls2)
    (cond [(empty? ls1) empty]
          [(empty? ls2) empty]
          [(eq? (first ls1) (first ls2)) (cons (first ls1) (iter (cdr ls1) (cdr ls2)))]
          [ else (iter (cdr ls1) (cdr ls2))]))

  (let* ([ls1 (string->list str1)]
         [ls2 (string->list str2)]
         [final-string (iter ls1 ls2)])
    (list->string final-string)))

(define lt (lower-triangle adv))

(define (map-edit lt-list)
  (map (curry simple-edit-distance (first (first lt-list))) (cdr lt-list))
  )

(define (remove-last ls)
  (reverse (cdr (reverse ls))))

(define (get-max-indexes lt)
  (let* ([result (map map-edit lt)]
         [result-tailless (remove-last result)]
         [result-lengths (map (lambda (le) (map string-length le)) result)]
         [result-clean (remove '() result-lengths)]
         [maxes (map (lambda (arg) (apply max arg)) result-clean)]
         [max-indexes (map (lambda (vec index) (index-of vec index)) result-clean maxes) ])
    (map list maxes max-indexes result-tailless)))

(define (add-index ls)
  (define (iter ls counter)
    (cond [(empty? ls) empty]
          [else (cons (append (first ls) counter) (iter (cdr ls) (+ counter 1)))]))
  (iter ls 0))

(define final-result (get-max-indexes lt))

(list-ref final-result 32)

(define (get-match result)
  (list-ref (third result) (second result)))

(get-match (list-ref final-result 32))



