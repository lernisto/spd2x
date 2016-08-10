;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:
#;
("PROBLEM A:

Design a data definition to represent a list of images. Call it ListOfImage.
")
;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of Images
(define I0 (circle 20 'solid 'red))
(define I1 (rectangle 20 20 'solid 'blue))
(define IL0 empty)
(define IL1 (cons I0 (cons I1 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfImage)
;;  - self-reference: (rest lon) is ListOfImage

;; =================
;; Functions:
#;
("
PROBLEM B:

Design a function that consumes a list of images and produces a number 
that is the sum of the areas of each image. For area, just use the image's 
width times its height.
")

;; ListOfImage -> Number
;; produce a Number representing the total area of the images
(check-expect (total-area IL0) 0)
(check-expect (total-area IL1) 2000)

;(define (total-area loi) 0);stub
;; Template from ListOfImage
(define (total-area loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-area (first loi))
              (total-area (rest loi)))]))



;; Image -> Number
;; produce a Number representing the area of the image (* width height)
(check-expect (image-area I0) (* 40 40)) ; 1600
(check-expect (image-area I1) (* 20 20)) ; 400

(define (image-area i)
  (* (image-width i) (image-height i)))
