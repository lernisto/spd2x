;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname making-rain-filtered) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered.rkt

#;
("
PROBLEM:

Design a simple interactive animation of rain falling down a screen. Wherever we click,
a rain drop should be created and as time goes by it should fall. Over time the drops
will reach the bottom of the screen and "fall off". You should filter these excess
drops out of the world state - otherwise your program is continuing to tick and
and draw them long after they are invisible.

In your design pay particular attention to the helper rules. In our solution we use
these rules to split out helpers:
  - function composition
  - reference
  - knowledge domain shift


NOTE: This is a fairly long problem.  While you should be getting more comfortable with
world problems there is still a fair amount of work to do here. Our solution has 9
functions including main. If you find it is taking you too long then jump ahead to the
next homework problem and finish this later.
")


;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 400)

(define SPEED 1)

(define SPRITE (ellipse 4 8 'solid 'blue))

(define MTS (rectangle WIDTH HEIGHT 'solid "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d)
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  world-next)     ; ListOfDrop -> ListOfDrop
            (to-draw  world-render))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 30 34 "button-down") (new-drop empty 30 34))
(check-expect (handle-mouse empty 30 34 "move") empty)

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse ws x y mevt)
  (cond [(mouse=? mevt "button-down") (new-drop ws x y)]
        [else ws]))

;; ListOfDrop Integer Integer -> ListOfDrop
;; create a new drop and add it to the list
;; (domain shift)
(check-expect (new-drop empty 30 34) (cons (make-drop 30 34) empty))

(define (new-drop lod x y)
  (cons (make-drop x y) lod))



;; List Predicate -> List
;; produce a new list with matching elements removed
(check-expect (filter-drop odd? empty) empty)
(check-expect (filter-drop odd? (list 1 2 3)) (list 2))

(define (filter-drop drop? lod)
  (cond [(empty? lod) empty]
        [else
         (if (drop? (first lod))
             (filter-drop drop? (rest lod))
             (cons (first lod)
                   (filter-drop drop? (rest lod))))]))


;; Drop -> Boolean
;; produce true if the drop is off the screen
;; !!!
(check-expect (off-screen? (make-drop 3 6)) #f)
(check-expect (off-screen? (make-drop 4 HEIGHT)) #f)
(check-expect (off-screen? (make-drop 4 (add1 HEIGHT))) #t)

(define (off-screen? drop) (> (drop-y drop) HEIGHT))

;; Drop -> Drop
;; move the drop to the next position
(check-expect (move-drop (make-drop 3 6))
              (make-drop 3 (+ 6 SPEED)))

(define (move-drop d)
  (make-drop (drop-x d)
             (+ SPEED (drop-y d))))


;; ListOfDrop -> ListOfDrop
;; advance each drop to the next position
(check-expect (move-drops empty) empty)
(check-expect (move-drops (list (make-drop 3 6))) (list (make-drop 3 (+ 6 SPEED))))

(define (move-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (move-drop (first lod))
               (move-drops (rest lod)))]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
;; (composition)
(check-expect (world-next empty) empty)
(check-expect (world-next
               (list (make-drop 3 6)
                     (make-drop 12 HEIGHT)))
              (list (make-drop 3 (+ 6 SPEED))))

(define (world-next lod)
  (filter-drop off-screen? (move-drops lod)))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (world-render empty) MTS)
(check-expect (world-render (list (make-drop 3 6) (make-drop 12 17)))
              (place-image SPRITE 3 6
                           (place-image SPRITE 12 17
                                        MTS)))


;(define (world-render lod) MTS) ; stub
(define (world-render lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image SPRITE
                      (drop-x (first lod))
                      (drop-y (first lod))
                      (world-render (rest lod)))]))
