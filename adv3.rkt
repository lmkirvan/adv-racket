#lang br
(require 2htdp/image)
(define in (open-input-file "/home/kirvanlewis/advent/advent-3.txt"))
(define adv (port->lines in))

(define (split-space string)
  (let ([curry-space (curryr string-split " " #:repeat? #t)])
  (curry-space string)))

(define example (split-space (list-ref adv 0)))

(define (int-seq from to by)
  (cond [(> from to) empty]
        [(eq? from to ) (list from)]
        [else (cons from (int-seq (+ from by) to by))]))

(define (parse-corner corner)
  (let* ([split (string-split corner ",")]
         [clean (map (curryr string-replace ":" "") split)])
    (map string->number clean)
  ))

(define (parse-size size)
    (let ([split (string-split size "x")])
    (map string->number split)
  ))

(define (square-coordinates coords)
  (let* ([id (first coords)]
        [corner (parse-corner (third coords))]
        [size (parse-size (fourth coords))]
        [to (map (lambda (li1 li2) (+ li1 li2))  corner size)]
        [to-cross (map (curryr int-seq 1) corner to)])
    (cartesian-product (first to-cross) (second to-cross))
  ))


(square 1 "solid" "red")


