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
(define I1 (square 20 'solid 'red))
(define I2 (triangle 23 'solid 'green))
(define I3 (circle 11 'solid 'blue))

(define R1 (rectangle 20 10 'solid 'red))
(define R2 (rectangle 20 20 'solid 'green))
(define R3 (rectangle 20 30 'solid 'blue))


(define L1 empty)
(define LI (list I1 I2 I3))
(define LR (list R1 R2 R3))
(define L2 (list I3 I2 I1)) 

#;
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


;; ListOfImage -> Image
;; lay out images left to right in increasing order of size
(check-expect (arrange-images L2) (layout-images (sort-images L2)))

;; Template from FunctionComposition
(define (arrange-images loi)
  (layout-images (sort-images loi)))



;; ListOfImage -> ListOfImage
;; lay out images left to right
(check-expect (layout-images L1) IBLANK)
(check-expect (layout-images (list I3 I1 I2))
              (beside I3 I1 I2 IBLANK))
(check-expect (layout-images (list I3 I2 I1))
              (beside I3 I2 I1 IBLANK))

(define (layout-images loi)
  (cond [(empty? loi) IBLANK]
        [else
         (beside (first loi)
                 (layout-images (rest loi)))]))



;; ListOfImage -> ListOfImage
;; sort images in increasing order of size
;; !!!
(check-expect (sort-images empty) empty)
(check-expect (sort-images (list R3 R2))
              (list R2 R3))
(check-expect (sort-images (list R2 R3))
              (list R2 R3))
(check-expect (sort-images (list R2 R3 R1))
              (list R1 R2 R3))

;(define (sort-images loi) loi) ; stub

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-images (rest loi)))]))

;; Image ListOfImage -> ListOfImage
;; produce new list with image in proper position (increasing order of area)
;; ASSUME: loi is already sorted
;; !!!
;(define (insert img loi) (cons img loi));stub
(check-expect (insert I1 empty) (list I1))
(check-expect (insert I2 (list I1)) (list I1 I2))
(check-expect (insert I1 (list I2)) (list I1 I2))
(check-expect (insert I2 (list I1 I3)) (list I1 I2 I3))

;(define (insert img loi) (cons img loi));stub
;; !!! is this a bubble sort? O(n²)
(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (larger? img (first loi))
             (cons (first loi) (insert img (rest loi)))
             (cons img loi))]))

;; Image Image -> Boolean
;; produce #t if first image smaller than second
(check-expect (larger? (rectangle 3 4 'solid 'red) (rectangle 2 6 'solid 'red)) #f)
(check-expect (larger? (rectangle 4 4 'solid 'red) (rectangle 2 6 'solid 'red)) #t)
(check-expect (larger? (rectangle 3 5 'solid 'red) (rectangle 2 6 'solid 'red)) #t)
(check-expect (larger? (rectangle 3 4 'solid 'red) (rectangle 3 6 'solid 'red)) #f)
(check-expect (larger? (rectangle 3 4 'solid 'red) (rectangle 2 7 'solid 'red)) #f)

(define (larger? a b)
  (> (image-area a) (image-area b)))

;; Image -> Natural
;; produce the area of the image (height*width)
(define (image-area img)
  (* (image-height img) (image-width img)))