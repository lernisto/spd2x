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
(define CTR-Y (/ HEIGHT 2))

;; =================
;; Data definitions:

(define-struct sprite (x y θ dx dy dθ))
;; Sprite is (make-sprite x y θ dx dy dθ )
;; - x Natural[0,WIDTH) screen x-coordinate in pixels
;; - y Natural[0,HEIGHT) screen y-coordinate in pixels
;; - θ - Number[0,360) angle of rotation in degrees
;; - dx Integer change in x in pixels per tick
;; - dy Integer change in y in pixels per tick
;; - dθ - Number[0,360) change in θ degrees per tick
(define S0 (make-sprite MIN-X CTR-Y 0 3 0 -3))
(define S1 (make-sprite MAX-X CTR-Y 0 -3 0  3))
(define S2 (make-sprite MIN-X 0 0 3 3 -3))

#;
(define (fn-for-sprite s)
  (...
   (sprite-x s) ; Natural[0,WIDTH]
   (sprite-y s) ; Natural[0,HEIGHT]
   (sprite-θ s) ; Number[0,360)
   (sprite-dx s) ; Integer
   (sprite-dy s) ; Integer
   (sprite-dθ s) ; Number[0,360)
   ))
;; template rules used
;; - compound: 6 fields
;; - simple atomic: Natural[0,WIDTH]
;; - simple atomic: Natural[0,HEIGHT]
;; - simple atomic: Number[0,360)
;; - simple atomic: Integer
;; - simple atomic: Integer
;; - simple atomic: Number[0,360)

;; =================
;; Functions:

;; Sprite -> Sprite
;; start the world with (main S0)
(define (main ws)
  (big-bang
   ws                        ; Sprite
   (on-tick   next-sprite)   ; Sprite -> Sprite
   (to-draw   render-sprite) ; Sprite -> Image
   (on-mouse  handle-mouse)  ; Sprite Integer Integer MouseEvent -> Sprite
   (on-key    handle-key)))  ; Sprite KeyEvent -> Sprite

;; Sprite -> Sprite
;; produce the next position for the sprite: x+=dx
;;   reverse direction when reaching a fence
(check-expect (next-sprite (make-sprite 80 0 0 3 2 -3)) (make-sprite 83 2 357 3 2 -3)) ;; normal motion
(check-expect (next-sprite (make-sprite MAX-X CTR-Y 0  3 0 -3)) (make-sprite MAX-X CTR-Y 0 -3 0 3)) ;; reverse at right end
(check-expect (next-sprite (make-sprite MIN-X CTR-Y 0 -3 0 -3)) (make-sprite MIN-X CTR-Y 0 3 0 3)) ;; reverse at left end

;(define (next-sprite ws) ws) ;stub

;; template from Sprite
(define (next-sprite s)
  (cond
    [(> (+ (sprite-x s) (sprite-dx s)) MAX-X)
     (make-sprite
      MAX-X
      (+ (sprite-y s) (sprite-dy s))
      (sprite-θ s) ;; !!! not exact
      (- (sprite-dx s))
      (sprite-dy s)
      (- (sprite-dθ s))
      )]
    [(< (+ (sprite-x s) (sprite-dx s)) MIN-X)
     (make-sprite
      MIN-X
      (+ (sprite-y s) (sprite-dy s))
      (sprite-θ s) ;; !!! not exact
      (- (sprite-dx s))
      (sprite-dy s)
      (- (sprite-dθ s))
      )]
    [else
     (make-sprite
      (+ (sprite-x s) (sprite-dx s))
      (+ (sprite-y s) (sprite-dy s))
      (modulo (+ (sprite-θ s) (sprite-dθ s)) 360)
      (sprite-dx s)
      (sprite-dy s)
      (sprite-dθ s)
      )]
    ))


;; Sprite -> Image
;; place the sprite image at x,CTR-Y on MTS
(check-expect (render-sprite S0) (place-image SPRITE (sprite-x S0) CTR-Y MTS))

;(define (render-sprite ws) MTS); stub

;; template from Sprite
(define (render-sprite s)
  (place-image (rotate (sprite-θ s) SPRITE)
               (sprite-x s)
               (sprite-y s)
               MTS))

;; Sprite KeyEvent -> Sprite
;; restart the animation when ' ' is pressed
(check-expect (handle-key S1 " ") S0)
(check-expect (handle-key S1 "a") S1)

;(define (handle-key ws ke) ws) ; stub

;; template from HtDW
(define (handle-key ws ke)
  (cond [(key=? ke " ") S0]
        [else ws]))

;; Sprite Integer Integer MouseEvent -> Sprite
;; reverse the direction of travel when mouse is clicked
(check-expect (handle-mouse S1 0 0 "button-down") (reverse-sprite S1))
(check-expect (handle-mouse S1 0 0 "move") S1)

;(define (handle-mouse ws x y me) ws) ; stub

;; template from HtDW
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (reverse-sprite ws)]
        [else ws]))

;; Sprite -> Sprite
;; reverse the direction of travel of the sprite
(check-expect (reverse-sprite (make-sprite 0 0 0 1 0 -1)) (make-sprite 0 0 0 -1 0 1))

;(define (reverse-sprite s) s);stub

;; template from Sprite
(define (reverse-sprite s)
  (make-sprite
   (sprite-x s)
   (sprite-y s)
   (sprite-θ s)
   (- (sprite-dx s))
   (sprite-dy s)
   (- (sprite-dθ s))
   ))
