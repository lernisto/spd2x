;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define QUIT-DELAY 3)

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

;; PositionChange -> PositionChange
;; produce a vector with the same value and opposite direction
(check-expect (xdx-freeze (make-xdx 3 -3)) (make-xdx 3 0))

;; Template from PositionChange
(define (xdx-freeze pc)
  (make-xdx
   (xdx-value pc)
   0))

;; PositionChange Number -> PositionChange
;; produce a vector with the same value and a direction increased by change
(check-expect (nudge (make-xdx 0 0) 5) (make-xdx 0 5))
(check-expect (nudge (make-xdx 0 0) -3) (make-xdx 0 -3))

(define (nudge pc change)
  (make-xdx (xdx-value pc) (+ (xdx-change pc) change)))


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
(define SS3 (make-3pos/flat 300 -1 300 1 0 -3))


;; SpriteState -> SpriteState
;; produce the next sprite state by applying xdx-next to each PositionChange
(check-expect (sprite-update SS0) SS1)

;(define (sprite-next s) s);stub
;; Template from SpriteState
(define (sprite-update 3p)
  (make-3pos (xdx-next (3pos-x 3p))
             (xdx-next (3pos-y 3p))
             (xdx-next (3pos-theta 3p))))


;; =================
;; World Functions:

;; Sprite -> Sprite
;; start the world with (main SS0)
(define (sprite-main ws)
  (big-bang
   ws                        ; Sprite
   (on-tick   sprite-update) ; Sprite -> Sprite
   (to-draw   sprite-render) ; Sprite -> Image
   (on-mouse  sprite-mouse)  ; Sprite Integer Integer MouseEvent -> Sprite
   (on-key    sprite-key)    ; Sprite KeyEvent -> Sprite
   (close-on-stop QUIT-DELAY); close window after a few seconds
   ))


;; SpriteState -> Image
;; render the sprite onto the MTS
(check-expect (sprite-render SS0)
              (sprite-render-on SS0 MTS))

;(define (render-sprite s) MTS);stub

;; Template from SpriteState
(define (sprite-render s)
  (sprite-render-on s MTS))

;; SpriteState Image -> Image
;; render the sprite onto the image
(check-expect (sprite-render-on SS0 MTS)
              (place-image
               (rotate (modulo (xdx-value (3pos-theta SS0)) 360)  SPRITE)
               (xdx-value (3pos-x SS0))
               (xdx-value (3pos-y SS0))
               MTS))

;; Template from SpriteState
(define (sprite-render-on s scene)
  (place-image
   (rotate (modulo (xdx-value (3pos-theta s)) 360)  SPRITE)
   (xdx-value (3pos-x s))
   (xdx-value (3pos-y s))
   scene))


;; SpriteState -> SpriteState
;; reverse all of the change vectors
(check-expect (reverse-sprite SS2) SS3)

;; Template from SpriteState
(define (reverse-sprite 3p)
  (make-3pos (xdx-reverse (3pos-x 3p))
             (xdx-reverse (3pos-y 3p))
             (xdx-reverse (3pos-theta 3p))))


(define SS4 (make-3pos/flat 12 1 13 -1 6 3))
(define SS4u (make-3pos/flat 12 1 13 -1 6 4))
(define SS4o (make-3pos/flat 12 1 13 -1 6 2))
(define SS4j (make-3pos/flat 12 0 13 -1 6 3))
(define SS4l (make-3pos/flat 12 2 13 -1 6 3))
(define SS4i (make-3pos/flat 12 1 13 -2 6 3))
(define SS4k (make-3pos/flat 12 1 13  0 6 3))
(define SS4-freeze (make-3pos/flat 12 0 13 0 6 0))

;; SpriteState -> SpriteState
;; nudge in the specified direction
(check-expect (nudge-counterclockwise SS4) SS4u)
(check-expect (nudge-clockwise SS4) SS4o)
(check-expect (nudge-up SS4) SS4i)
(check-expect (nudge-down SS4) SS4k)
(check-expect (nudge-left SS4) SS4j)
(check-expect (nudge-right SS4) SS4l)

(define (nudge-clockwise s)
  (make-3pos (3pos-x s)
             (3pos-y s)
             (nudge (3pos-theta s) -1)))

(define (nudge-counterclockwise s)
  (make-3pos (3pos-x s)
             (3pos-y s)
             (nudge (3pos-theta s) 1)))

(define (nudge-up s)
  (make-3pos (3pos-x s)
             (nudge (3pos-y s) -1)
             (3pos-theta s)))

(define (nudge-down s)
  (make-3pos (3pos-x s)
             (nudge (3pos-y s) 1)
             (3pos-theta s)))

(define (nudge-left s)
  (make-3pos (nudge (3pos-x s) -1)
             (3pos-y s)
             (3pos-theta s)))

(define (nudge-right s)
  (make-3pos (nudge (3pos-x s) 1)
             (3pos-y s)
             (3pos-theta s)))

;; SpriteState -> SpriteState
;; stop all motion for sprite
(check-expect (freeze-sprite SS4) SS4-freeze)

;(define (freeze-sprite s) s);stub
(define (freeze-sprite s)
  (make-3pos
   (xdx-freeze (3pos-x s))
   (xdx-freeze (3pos-y s))
   (xdx-freeze (3pos-theta s))))


;; ==============
;; event handlers

;; Sprite KeyEvent -> Sprite
(check-expect (sprite-key SS2 "q") (stop-with SS2))
(check-expect (sprite-key SS2 "r") (reverse-sprite SS2))
(check-expect (sprite-key SS4 " ") (freeze-sprite SS4))
(check-expect (sprite-key SS4 "u") SS4u)
(check-expect (sprite-key SS4 "o") SS4o)
(check-expect (sprite-key SS4 "i") SS4i)
(check-expect (sprite-key SS4 "k") SS4k)
(check-expect (sprite-key SS4 "j") SS4j)
(check-expect (sprite-key SS4 "l") SS4l)

(check-expect (sprite-key SS2 "a") SS2)

;(define (sprite-key ws ke) ws) ; stub

;; template from HtDW
(define (sprite-key ws ke)
  (cond [(key=? ke "r") (reverse-sprite ws)]
        [(key=? ke " ") (freeze-sprite ws)]
        [(key=? ke "u") (nudge-counterclockwise ws)]
        [(key=? ke "o") (nudge-clockwise ws)]
        [(key=? ke "i") (nudge-up ws)]
        [(key=? ke "k") (nudge-down ws)]
        [(key=? ke "j") (nudge-left ws)]
        [(key=? ke "l") (nudge-right ws)]
        [(key=? ke "q") (stop-with ws)]
        [else ws]))

;; Sprite Integer Integer MouseEvent -> Sprite
;; reverse the direction of travel when mouse is clicked
(check-expect (sprite-mouse SS2 0 0 "button-down") (make-3pos/flat 0 0 0 0 0 3) )
(check-expect (sprite-mouse SS2 0 0 "move") SS2)

;(define (sprite-mouse ws x y me) ws) ; stub

;; template from HtDW
(define (sprite-mouse ws x y me)
  (cond [(mouse=? me "button-down") (make-3pos/flat x 0 y 0 0 3)]
        [else ws]))







;; WorldState is one of:
;; - empty
;; - (cons SpriteState WorldState)
(define WS0 empty)
(define WS1 (cons SS4 (cons SS3 empty)))

#;
(define (fn-for-ws ws)
  (cond [(empty? ws) (...)]
        [else
         (... (fn-for-3pos (first ws))
              (fn-for-ws (rest ws)))]))
;; Template rules used:
;; - one of: 2 cases
;; - simple atomic: empty
;; - compound: (cons SpriteState WorldState)
;; - reference: (first ws) is SpriteState
;; - self-reference: (rest ws) is WorldState

;; WorldState -> WorldState
;; apply sprite-next to all members
;; !!!
(check-expect (world-update WS0) WS0)
(check-expect (world-update WS1)
              (cons (sprite-update (first WS1))
                    (cons (sprite-update (second WS1))
                          empty)))


;(define (world-update ws) ws);stub
;; Template from WorldState
(define (world-update ws)
  (cond [(empty? ws) ws]
        [else
         (cons
          (sprite-update (first ws))
          (world-update (rest ws)))]))

;; WorldState -> Image
;; render all sprites into MTS
;; !!!
(check-expect (world-render WS0) MTS)
(check-expect (world-render WS1)
              (sprite-render-on (first WS1)
                                (sprite-render-on (second WS1)
                                                  MTS)))

;(define (world-render ws) MTS); stub
;; Template from WorldState
(define (world-render ws)
  (cond [(empty? ws) MTS]
        [else
         (sprite-render-on (first ws)
                           (world-render (rest ws)))]))

;; WorldState Integer Integer MouseEvent -> WorldState
;; create a new sprite at the mouse position
(check-expect (world-mouse WS0 60 70 "button-down")
              (cons (make-3pos/flat 60 0 70 0 0 3) WS0))
(check-expect (world-mouse WS1 80 90 "button-down")
              (cons (make-3pos/flat 80 0 90 0 0 3) WS1))

;(define (world-mouse ws x y me) ws);stub
;; Template from WorldState
(define (world-mouse ws x y me)
  (cond [(mouse=? me "button-down") (cons (make-3pos/flat x 0 y 0 0 3) ws)]
        [else ws]))

;; WorldState KeyEvent -> WorldState
;; control the top-most sprite
;; !!!
(check-expect (world-key WS0 " ") WS0)
(check-expect (world-key WS0 "q") (stop-with WS0))
(check-expect (world-key (cons SS4 empty) " ") (cons SS4-freeze empty))

;(define (world-key ws ke) ws); stub
;; Template from WorldState
(define (world-key ws ke)
  (cond [(key=? ke "q") (stop-with ws)]
        [(empty? ws) ws]
        [else
         (cons (sprite-key (first ws) ke)
               (rest ws))]))


;; WorldState -> WorldState
;; start the world with (world-main WS0)
(define (world-main ws)
  (big-bang
   ws                        ; WorldState
   (on-tick   world-update)  ; WorldState -> WorldState
   (to-draw   world-render)  ; WorldState -> Image
   (on-mouse  world-mouse)   ; WorldState Integer Integer MouseEvent -> WorldState
   (on-key    world-key)     ; WorldState KeyEvent -> WorldState
   (close-on-stop QUIT-DELAY); close window after a few seconds
   ))
