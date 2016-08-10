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
(define WIDTH 800)
(define HEIGHT 800)
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


(define-struct 3pos (x y theta))
;; SpriteState is (make-3pos x y theta)
;; interp. value and change per second for x coordinate, y coordinate, and orientation
(define SS0 (make-3pos (make-xdx 0 3) (make-xdx 0 1) (make-xdx  0 -3)))
(define SS1 (make-3pos (make-xdx 3 3) (make-xdx 1 1) (make-xdx -3 -3)))

#;
(define (fn-for-3pos 3p)
  (... (fn-for-pc (3pos-x 3p))
       (fn-for-pc (3pos-y 3p))
       (fn-for-pc (3pos-theta 3p))))
;; Template rules used:
;; -- compound: 3 fields
;; -- reference: 3pos-x is PositionChange
;; -- reference: 3pos-y is PositionChange
;; -- reference: 3pos-theta is PositionChange

;; Number Number Number Number Number Number -> SpriteState
;; a convenient constructor
(check-expect (make-3pos/flat 0 3 0 1 0 -3) SS0)

;(define (make-3pos/flat x dx y dy t dt) #f); stub
;; Template from my head
(define (make-3pos/flat x dx y dy t dt)
  (make-3pos (make-xdx x dx)
             (make-xdx y dy)
             (make-xdx t dt)))

(define SS2 (make-3pos/flat 300 1 300 -1 0 3))


;; SpriteState -> SpriteState
;; produce the next sprite state by applying xdx-next to each PositionChange
(check-expect (sprite-next SS0) SS1)

;(define (sprite-next s) s);stub
;; Template from SpriteState
(define (sprite-next 3p)
  (make-3pos (xdx-next (3pos-x 3p))
             (xdx-next (3pos-y 3p))
             (xdx-next (3pos-theta 3p))))


;; =================
;; World Functions:

;; Sprite -> Sprite
;; start the world with (main SS0)
(define (main ws)
  (big-bang
   ws                        ; Sprite
   (on-tick   sprite-next)   ; Sprite -> Sprite
   (to-draw   render-sprite) ; Sprite -> Image
   #;(on-mouse  handle-mouse)  ; Sprite Integer Integer MouseEvent -> Sprite
   #;(on-key    handle-key)))  ; Sprite KeyEvent -> Sprite


;; SpriteState -> SpriteState
;; produce the next state for a single sprite
;; !!! no bound checking
#;(define next-sprite sprite-next) ;; stub for single sprite version

;; SpriteState -> Image
;; render the sprite onto the MTS
(check-expect (render-sprite SS0)
              (place-image
               (rotate (modulo (xdx-value (3pos-theta SS0)) 360)  SPRITE)
               (xdx-value (3pos-x SS0))
               (xdx-value (3pos-y SS0))
               MTS))

;(define (render-sprite s) MTS);stub

;; Template from SpriteState
(define (render-sprite s)
  (place-image
   (rotate (modulo (xdx-value (3pos-theta s)) 360)  SPRITE)
   (xdx-value (3pos-x s))
   (xdx-value (3pos-y s))
   MTS))