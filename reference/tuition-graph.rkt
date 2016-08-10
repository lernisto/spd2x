;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tuition-graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; tuition-graph.rkt

#;
("PROBLEM:

Eva is trying to decide where to go to university. One important factor for her is 
tuition costs. Eva is a visual thinker, and has taken Systematic Program Design, 
so she decides to design a program that will help her visualize the costs at 
different schools. She decides to start simply, knowing she can revise her design
later.

The information she has so far is the names of some schools as well as their 
international student tuition costs. She would like to be able to represent that
information in bar charts like this one:

        <img src=example-chart.png />
        
(A) Design data definitions to represent the information Eva has.
(B) Design a function that consumes information about schools and their
    tuition and produces a bar chart.
(C) Design a function that consumes information about schools and produces
    the school with the lowest international student tuition.
    ")

(require 2htdp/image)

;; =========
;; Constants

(define BAR-WIDTH 30)
(define CHART-HEIGHT 200)
(define BAR-COLOR 'aliceblue)
(define CHART-COLOR 'white)
(define TEXT-SIZE 20)
(define TEXT-COLOR 'darkblue)
(define Y-SCALE 1/50) ;; TODO calculate this as CHART_HEIGHT/max-tuition

(define EMPTY-CHART (square 0 'solid BAR-COLOR))

;; ================
;; Data Definitions

(define-struct school (name tuition))
;; School is (make-school name tuition)
;; - compound: 2 fields
;; - atomic distinct: String name - interp. name of school for chart
;; - atomic distinct: Number tutition - interp. annual tuition in US dollars
(define S1 (make-school "BYU Provo" (* 3 2500))) ;; oops. these are not international student fees, but are relevant to me
(define S2 (make-school "BYU Idaho" (* 3 1915)))
(define S3 (make-school "WGU" (* 2 3000)))

#;
(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))
;; Template rules used:
;;  - compound: 2 fields

;; ListOfSchool is one of:
;; - empty
;; - (cons School ListOfSchool)
(define LS1 empty)
(define LS2 (cons S1 (cons S2 (cons S3 empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons School ListOfSchool)
;;  - reference: (first los) is School
;;  - self-reference: (rest los) is ListOfSchool

;; =========
;; Functions

;; ListOfSchool -> Image
;; produce a bar chart of tuition for each school
(check-expect (chart empty) EMPTY-CHART)
(check-expect (chart (cons S3 empty)) (bar S3))
(check-expect (chart (cons S2 (cons S3 empty))) (beside/align 'bottom (bar S2)(bar S3)))

;(define (chart los) EMPTY-CHART);stub

;; Template from ListOfSchool
(define (chart los)
  (cond [(empty? los) EMPTY-CHART]
        [else
         (beside/align 'bottom (bar (first los))
              (chart (rest los)))]))


;; School -> Image
;; produce a bar representing the tuition of the school
(check-expect (bar (make-school "A" 5000))
              (overlay/align 'center 'bottom
               (rotate 90 (text "A" TEXT-SIZE TEXT-COLOR))
               (rectangle BAR-WIDTH (* 5000 Y-SCALE) 'outline 'black)
               (rectangle BAR-WIDTH (* 5000 Y-SCALE) 'solid BAR-COLOR)
               ))

;(define (bar s) EMPTY-CHART); stub

;; Template from School
(define (bar s)
  (overlay/align 'center 'bottom
   (rotate 90 (text (school-name s) TEXT-SIZE TEXT-COLOR))
   (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) 'outline 'black)
   (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) 'solid BAR-COLOR)))
