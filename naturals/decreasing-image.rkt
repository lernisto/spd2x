;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; decreasing-image.rkt

(define TEXT-SIZE 20)
(define TEXT-COLOR 'black)

(define (n->image n) (text (number->string n) TEXT-SIZE TEXT-COLOR))

#;
("
PROBLEM:
 
 Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
 from n to 0 side by side. 
 
 So (decreasing-image 3) should produce (n->image 3210)
")

;; Natural -> Image
;; produce an image of all natural numbers less than or equal to n, in decreasing order
(check-expect (decreasing-image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (decreasing-image 3) (text "3210" TEXT-SIZE TEXT-COLOR))

(define (decreasing-image n)
  (cond [(zero? n) (n->image 0) ]
        [else
         (beside (n->image n)
          (decreasing-image (sub1 n)))]))


