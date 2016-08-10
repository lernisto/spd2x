;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quidditch) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; quidditch.rkt

#;
("PROBLEM:

Imagine that you are designing a program that will keep track of
your favorite Quidditch teams. (http://www.internationalquidditch.org/).

Design a data definition to represent a list of Quidditch teams.
")

;; ListofString is one of:
;;  - empty
;;  - (cons String ListofString)
;; interp. a list of String
(define LOS1 empty)
(define LOS2 (cons "McGill" empty))
(define LOS3 (cons "UBC" LOS2))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los))
              )]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListofString)
;;  - self-reference: (rest los) is ListofString

#;
("
PROBLEM:

We want to know whether your list of favorite Quidditch teams includes
UBC! Design a function that consumes ListOfString and produces true if 
the list includes \"UBC\".
")
;; ListofString -> Boolean
;; produce true iif los contains "UBC"
(check-expect (contains-ubc? LOS1) #f)
(check-expect (contains-ubc? LOS2) #f)
(check-expect (contains-ubc? LOS3) #t)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) #t)


;(define (contains-ubc? los) #f);stub

;; template from ListofString
#;
(define (contains-ubc? los)
  (cond [(empty? los) #f]
        [(string=? "UBC" (first los)) #t]
        [else (contains-ubc? (rest los))]
        ))

(define (contains-ubc? los)
  (cond [(empty? los) #f]
        [else
         (if (string=? "UBC" (first los))
             #t
             (contains-ubc? (rest los)))]))

