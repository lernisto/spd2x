;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname double-all) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; double-starter.rkt

;; =================
;; Data definitions:
#;
("
Remember the data definition for a list of numbers we designed in Lecture 5f:
(if this data definition does not look familiar, please review the lecture)
")

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:
#;("
PROBLEM:

Design a function that consumes a list of numbers and doubles every number 
in the list. Call it double-all.
")
;; ListOfNumber -> ListOfNumber
;; produce a copy of the list with each element doubled
(check-expect (double-all empty) empty)
(check-expect (double-all (cons 6 (cons 3 empty))) (cons 12 (cons 6 empty)))

;(define (double-all lon) lon);stub
;; Template from ListOfNumber
(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* 2 (first lon))
              (double-all (rest lon)))]))