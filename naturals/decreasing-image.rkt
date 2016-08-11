;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; decreasing-image-starter.rkt
#;
("
PROBLEM:
 
 Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
 from n to 0 side by side. 
 
 So (decreasing-image 3) should produce
")

(check-expect (decreasing-image 3) (text "3210" 20 'black))



