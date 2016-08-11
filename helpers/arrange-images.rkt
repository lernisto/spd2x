;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; arrange-images.rkt

#;
("PROBLEM:

In this problem imagine you have a bunch of pictures that you would like to 
store as data and present in different ways. We'll do a simple version of that 
here, and set the stage for a more elaborate version later.

(A) Design a data definition to represent an arbitrary number of images.

(B) Design a function called arrange-images that consumes an arbitrary number
    of images and lays them out left-to-right in increasing order of size.
")

;; =========
;; Constants
(define IBLANK (square 0 'solid 'white))

;; ================
;; Data Definitions

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; inter. a list of Image
(define I1 (circle 10 'solid 'blue))
(define I2 (square 20 'solid 'red))
(define I3 (triangle 23 'solid 'green))

(define L1 empty)
(define L2 (cons I1 (cons I2 (cons I3 empty))))
(define L3 (list I1 I2 I3))

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))
;; Template rules used
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self ref: (rest loi) is ListOfImage


;; =========
;; Functions


;; ListOfImage -> ListOfImage
;; lay out 