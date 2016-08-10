;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; spinning-bears.rkt

(require 2htdp/image)
(require 2htdp/universe)

#;
("PROBLEM:

In this problem you will design another world program. In this program the changing 
information will be more complex - your type definitions will involve arbitrary 
sized data as well as the reference rule and compound data. But by doing your 
design in two phases you will be able to manage this complexity. As a whole, this problem 
will represent an excellent summary of the material covered so far in the course, and world 
programs in particular.

This world is about spinning bears. The world will start with an empty screen. Clicking
anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
screen, a new upright bear appears and starts spinning.

So each bear has its own x and y position, as well as its angle of rotation. And there are an
arbitrary amount of bears.

To start, design a world that has only one spinning bear. Initially, the world will start
with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
world will replace the old bear with a new bear at the new spot. You can do this part 
with only material up through compound. 

Once this is working you should expand the program to include an arbitrary number of bears.
")

;; Rolling sprite animation

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 200)
(define MTS (empty-scene WIDTH HEIGHT))
(define RADIUS 40)
(define SPRITE
  (bitmap "bear.png")
  #;(overlay
     (radial-star 5 (* RADIUS 1/2) (* RADIUS 1) 'solid 'white)
     (circle RADIUS 'solid 'blue)))
(define MIN-X (/ (image-width SPRITE) 2))
(define MAX-X (- WIDTH (/ (image-width SPRITE) 2)))
(define MIN-Y (/ (image-height SPRITE) 2))
(define MAX-Y (- WIDTH (/ (image-height SPRITE) 2)))
(define CTR-Y (/ HEIGHT 2))

;; =================
;; Data definitions:

(define-struct interval (low high))
;; Interval is (make-interval Number Number)
;; interp. a lower and upper bound for Number
(define IX (make-interval MIN-X MAX-Y))
(define IY (make-interval MIN-Y MAX-Y))
#;
(define (fn-for-interval int)
  (... (interval-low int)
       (interval-high int)))
;; Template rules used:
;; -- compound: 2 fields

;; Functions for Interval

;; Interval -> Number
;; produce the midpoint of the low and high values
(check-expect (midpoint (make-interval 0 4)) 2)
(check-expect (midpoint (make-interval -4 4)) 0)
(check-expect (midpoint (make-interval 2 4)) 3)

;(define (midpoint i) 0);stub

;; Template from Interval
(define (midpoint int)
  (* 1/2 (+ (interval-low int)
            (interval-high int))))

;; Interval Number -> Boolean
;; produce true if number is below the lower bound
(check-expect (below? (make-interval 100 200) 0 ) #t)
(check-expect (below? (make-interval 100 200) 150 ) #f)
(check-expect (below? (make-interval 100 200) 300 ) #f)

;(define (below? i n) #f);stub
;; Template from Interval
(define (below? int n)
  (< n (interval-low int)))

;; Interval Number -> Boolean
;; produce true if number is between (inclusive) the lower and upper bounds
(check-expect (within? (make-interval 100 200) 0 ) #f)
(check-expect (within? (make-interval 100 200) 100 ) #t)
(check-expect (within? (make-interval 100 200) 150 ) #t)
(check-expect (within? (make-interval 100 200) 200 ) #t)
(check-expect (within? (make-interval 100 200) 300 ) #f)

;; Template from Interval
(define (within? int n)
  (and (>= n (interval-low int))
       (<= n (interval-high int))))


;; Interval Number -> Boolean
;; produce true if number is between (exclusive) the lower and upper bounds
(check-expect (between? (make-interval 100 200) 0 ) #f)
(check-expect (between? (make-interval 100 200) 100 ) #f)
(check-expect (between? (make-interval 100 200) 150 ) #t)
(check-expect (between? (make-interval 100 200) 200 ) #f)
(check-expect (between? (make-interval 100 200) 300 ) #f)

;; Template from Interval
(define (between? int n)
  (and (> n (interval-low int))
       (< n (interval-high int))))


;; Interval Number -> Boolean
;; produce true if number is above the upper bound
(check-expect (above? (make-interval 100 200) 0 ) #f)
(check-expect (above? (make-interval 100 200) 150 ) #f)
(check-expect (above? (make-interval 100 200) 300 ) #t)

;; Template from Interval
(define (above? int n)
  (> n (interval-high int)))

;; !!! (all-within? Interval Interval) -> Boolean
;; !!! (interval-intersect Interval Interval) -> Interval
;; !!! (interval-union? ListOfInterval) -> ListOfInterval
;; !!! interval trees


(define-struct xdx (value change))
;; PositionChange is (make-xdx value change)
;; interp. an instantaneous value and the change in that value in units per second
(define PC0 (make-xdx 3 -3))
(define PC1 (make-xdx 0 1))

(define (fn-for-pc pc)
  (.. (xdx-value pc)
      (xdx-change pc)))
;; Template rules used:
;; -- compound: 2 fields

;; PositionChange -> PositionChange
;; produce the next value in the progression
(check-expect (xdx-next (make-xdx 3 -3)) (make-xdx 0 -3))

;; Template from PositionChange
(define (xdx-next pc)
  (make-xdx
   (+ (xdx-value pc) (xdx-change pc))
   (xdx-change pc)))

;; PositionChange -> PositionChange
;; produce a vector with the same value and opposite direction
(check-expect (xdx-reverse (make-xdx 3 -3)) (make-xdx 3 3))

;; Template from PositionChange
(define (xdx-reverse pc)
  (make-xdx
   (xdx-value pc) 
   (- (xdx-change pc))))
