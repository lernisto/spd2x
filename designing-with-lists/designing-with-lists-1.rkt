;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname designing-with-lists-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; designing-with-lists-1.rkt

#;
("PROBLEM:

You've been asked to design a program having to do with all the owls
in the owlery.

(A) Design a data definition to represent the weights of all the owls. 
    For this problem call it ListOfNumber.
(B) Design a function that consumes the weights of owls and produces
    the total weight of all the owls.
(C) Design a function that consumes the weights of owls and produces
    the total number of owls.
")


;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
(define LON1 empty)
(define LON2 (cons 2 empty))
(define LON3 (cons 3 LON2))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else (... (first lon)
                   (fn-for-lon (rest lon)))]))
;; template rules used:
;; - compound: 2 cases
;; - simple atomic: empty
;; - compound: (cons Number ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber


;; ListOfNumber -> Number
;; produce the sum of all Numbers in the List
(check-expect (sum empty) 0)
(check-expect (sum (cons 3.5 empty)) 3.5)
(check-expect (sum (cons 4 (cons 3.5 empty))) 7.5)

;(define (sum lon) 0);stub

;; template from ListOfNumber
(define (sum lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon)
                 (sum (rest lon)))]))


;; ListOfNumber -> Number
;; produce the count of all Numbers in the List
(check-expect (count empty) 0)
(check-expect (count (cons 3.5 empty)) 1)
(check-expect (count (cons 4 (cons 3.5 empty))) 2)

;(define (count lon) 0);stub

;; template from ListOfNumber
(define (count lon)
  (cond [(empty? lon) 0]
        [else (+ 1
                 (count (rest lon)))]))
