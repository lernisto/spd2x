;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; boolean-list-starter.rkt

;; =================
;; Data definitions:
#;
("PROBLEM A:

Design a data definition to represent a list of booleans. Call it ListOfBoolean.
")
;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
(define LOB0 empty)
(define LOB1 (cons #t (cons #f empty)))
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is ListOfBoolean)

;; =================
;; Functions:
#;
("PROBLEM B:

Design a function that consumes a list of boolean values and produces true 
if every value in the list is true. If the list is empty, your function 
should also produce true. Call it all-true?
")
;; ListOfBoolean -> Boolean
;; produce true iff every element is true
(check-expect (all-true? empty) #t)
(check-expect (all-true? (cons #t (cons #t empty))) #t)
(check-expect (all-true? (cons #t (cons #f empty))) #f)
(check-expect (all-true? (cons #f (cons #t empty))) #f)

;(define (all-true? lob) #f);stub

;; Template from ListOfBoolean
#;
(define (all-true? lob)
  (cond [(empty? lob) #t]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))

;; This version seems to keep a smaller stack, but more steps in stepper
;
(define (all-true? lob)
  (cond [(empty? lob) #t]
        [(false? (first lob)) #f]
        [else (all-true? (rest lob))]))
