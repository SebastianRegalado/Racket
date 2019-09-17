;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname drawings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 07, Q3
;; *****************************************
;;

(require "a07drawinglib.rkt")

;;==== Data Definitions for parts C and on======================================

;; A Point is a (list Int Int)
;; An Offset is a Point

;; A ShapeID is a Sym
;; requires: ShapeID is not 'circle, 'triangle, 'rectangle, 'component

;; A Shape is one of:
;; - (list 'circle ShapeID radius ImageColor)
;; - (list 'triangle ShapeID Point Point Point ImageColor)
;; - (list 'rectangle ShapeID width height ImageColor)
;; - (list 'component ShapeID Picture)
;; requires: radius,width,height are Nat
;;   The ShapeID of a component does not appear in its Picture
;;   when recursively expanded.
;;   (i.e. there are no circular definitions)

;; A Picture is a (listof (list Offset ShapeID))

;; A NEPicture is a Picture
;; requires: NEPicture is non-empty

;; A SPicture is a (first NEPicture)

;; A ShapeList is a (listof Shape)
;; requires: every ID in the ShapeList is unique

;; A BundledDrawing is a (list width height Picture ShapeList)
;; requires: width, height are Nat
;;   Every ShapeID in the Picture occurs in ShapeList.


;; ==== Example drawings =======================================================

(define overlap-prims
  (list
   (make-prim-circle (make-posn 30 30) 4 "White")
   (make-prim-triangle
    (make-posn 10 40) (make-posn 50 40) (make-posn 30 10) "DarkBlue")
   (make-prim-circle (make-posn 30 30) 15 "Chocolate")))

(define summer-shapes '((circle top-scoop 10 "Pink")
                        (circle bottom-scoop 10 "LightBlue")
                        (component ice-cream
                                   (((0 40) cone)
                                    ((10 35) bottom-scoop)
                                    ((10 25) top-scoop)))
                        (triangle cone (0 0) (20 0) (10 50) "Burlywood")
                        (circle moon 35 "Light Gray")
                        (circle sun 40 "Yellow")))

(define ice-cream-pic '(((10 50) ice-cream)
                        ((70 20) sun)
                        ((130 30) ice-cream)))

(define ice-cream-drawing (list 200 150 ice-cream-pic summer-shapes))


;; ==== Question 3a ============================================================

(define prim-picture
  (list
   (make-prim-circle (make-posn 130 100) 20 "Brown")
   (make-prim-circle (make-posn 142 135) 17 "Brown")
   (make-prim-circle (make-posn 160 159) 13 "Brown")
   (make-prim-circle (make-posn 167 183) 12 "Brown")
   (make-prim-circle (make-posn 179 199) 8 "Brown")
   (make-prim-triangle
    (make-posn 146 149) (make-posn 200 30) (make-posn 280 130) "Orange")
   (make-prim-triangle
    (make-posn 146 149) (make-posn 275 140) (make-posn 240 220) "Orange")
   (make-prim-triangle
    (make-posn 140 153) (make-posn 25 50) (make-posn 5 195) "Orange")
   (make-prim-triangle
    (make-posn 140 153) (make-posn 35 210) (make-posn 140 260) "Orange")))

;; (render-image (make-posn 300 300) prim-picture)
;; (save-image (render-image (make-posn 300 300) prim-picture) "prim-bonus.png")


;; ==== Question 3b ============================================================

;; (rect top-left dimensions color) constructs a rectangle with offset top-left
;;   given dimensions and color
;; rect: Coordinate Coordinate ImageColor -> (listof PrimTriangles)
;; Example
(check-expect (rect (make-posn 0 0) (make-posn 30 60) "Blue")
              (list
               (make-prim-triangle (make-posn 0 0) (make-posn 30 0)
                                   (make-posn 30 60) "Blue")
               (make-prim-triangle (make-posn 0 0) (make-posn 30 60)
                                   (make-posn 0 60) "Blue")))

(define (rect top-left dimensions color)
  (local [(define width (posn-x dimensions))
          (define height (posn-y dimensions))
          (define x (posn-x top-left))
          (define y (posn-y top-left))]
    
    (list (make-prim-triangle top-left
                              (make-posn (+ width x) y)
                              (make-posn (+ width x) (+ height y))
                              color)
          (make-prim-triangle top-left
                              (make-posn (+ width x) (+ height y))
                              (make-posn x (+ height y))
                              color))))

;; Tests
(check-expect (rect (make-posn 20 30) (make-posn 23 54) "Red")
              (list
               (make-prim-triangle (make-posn 20 30) (make-posn 43 30)
                                   (make-posn 43 84) "Red")
               (make-prim-triangle (make-posn 20 30) (make-posn 43 84)
                                   (make-posn 20 84) "Red")))


;; ==== Question 3c ============================================================

(define fun-shapes '((circle circulin-1 12 "Red")
                     (circle circulin-2 15 "Green")
                     (triangle tri-force-1 (5 5) (0 10) (10 10) "Blue")
                     (triangle tri-force-2 (5 10) (0 15) (10 15) "Yellow")
                     (triangle tri-force-3 (3 3) (2 7) (8 4) "Pink")
                     (triangle tri-force-4 (2 3) (6 9) (56 4) "Snow")
                     (component triforce
                                (((0 2) triforce-1)
                                 ((4 3) triforce-2)
                                 ((5 8) triforce-3)))
                     (component figurilla
                                (((4 5) triforce)
                                 ((6 0) circulin)))
                     (component picturilla
                                (((2 2) triforce)
                                 ((1 1) figurilla)
                                 ((3 0) tri-force-4)
                                 ((5 5) circulin-2)))))

(define fun-pic '(((0 0) triforce)
                  ((0 0) figurilla)
                  ((0 0) picturilla)
                  ((50 50) circulin-2)
                  ((60 60) tri-force-3)))

(define fun-drawing (list 300 300 fun-pic fun-shapes))


;; ==== Question 3d ============================================================

;; (list-ids lst) produces a list containg all the ShapeIDs in lst
;; list-ids: Picture -> (listof ShapeID)
;; Examples:
(check-expect (list-ids empty) empty)
(check-expect (list-ids  '(((0 0) triforce))) '(triforce))

(define (list-ids lst)
  (cond [(empty? lst) empty]
        [else (cons (shape-id (first lst)) (list-ids (rest lst)))]))


;; (no-duplicates lst) removes the duplicates in lst
;; no-duplicates: (listof Any) -> (listof Any)
;; Examples:
(check-expect (no-duplicates empty) empty)
(check-expect (no-duplicates '(1 1 4 5)) '(1 4 5))

(define (no-duplicates lst)
  (cond [(empty? lst) empty]
        [(member? (first lst) (rest lst)) (no-duplicates (rest lst))]
        [else (cons (first lst) (no-duplicates (rest lst)))]))


;; (get-loids-ids lst-ids a-shapelist) produces a list of ShapeIDs that occur
;;   in each ShapeID of lst-ids
;; get-loids-ids: (listof ShapeID) ShapeList -> (listof ShapeID)
;; Example:
(check-expect (get-loids-ids '(top-scoop) summer-shapes) '(top-scoop))

(define (get-loids-ids lst-ids a-shapelist)
  (cond [(empty? lst-ids) empty]
        [(member? (first lst-ids) (list-ids a-shapelist))
         (append (get-ids-in-id (first lst-ids) a-shapelist)
                 (get-loids-ids (rest lst-ids) a-shapelist))]
        [else (cons (first lst-ids)
                    (get-loids-ids (rest lst-ids) a-shapelist))]))


;; (get-ids-in-id id a-shapelist) produces a list of ShapeIDs that occur
;;   in id
;; get-ids-in-id: ShapeID ShapeList -> (listof ShapeID)
;; Example:
(check-expect (get-ids-in-id 'top-scoop summer-shapes) '(top-scoop))

(define (get-ids-in-id id a-shapelist)
  (cond [(not (symbol=? id (shape-id (first a-shapelist))))
         (get-ids-in-id id (rest a-shapelist))]
        [(symbol=? 'component (shape-type (first a-shapelist)))
         (cons id (get-loids-ids
                        (list-ids (component-picture (first a-shapelist)))
                        a-shapelist))]
        [else (cons id empty)]))


;; (get-picture-ids a-picture a-shapelist) produces a list of ShapeIDs that
;;   occur in each a-picture (taking a-shapelist as reference) with no
;;   duplicates
;; get-picture-ids: Picture ShapeList -> (listof ShapeID)
;; Example:
(check-expect (get-picture-ids ice-cream-pic summer-shapes)
              (list 'sun 'ice-cream 'cone 'bottom-scoop 'top-scoop))

(define (get-picture-ids a-picture a-shapelist)
  (no-duplicates (get-loids-ids (no-duplicates (list-ids a-picture))
                                a-shapelist)))

;; Tests
(check-expect (get-picture-ids '(((0 2) figurilla)) fun-shapes)
              (list 'figurilla 'triforce 'circulin))
(check-expect (get-picture-ids '(((0 2) figurilla) ((7 0) picturilla))
                               fun-shapes)
              (list 'circulin 'picturilla 'triforce 'figurilla
                    'tri-force-4 'circulin-2))


;; ==== Question 3e ============================================================

;; (add-offset offset a-picture) increases the offsets of the elements in
;;   a-picture by offset
;; add-offset: Offset Picture -> Picture
;; Example:
(check-expect (add-offset '(1 1) '(((1 1) hi) ((2 2) ok)))
              '(((2 2) hi) ((3 3) ok)))

(define (add-offset offset a-picture)
  (cond [(empty? a-picture) empty]
        [else
         (cons
          (list (list (+ (point-x offset) (point-x (first (first a-picture))))
                      (+ (point-y offset) (point-y (first (first a-picture)))))
                (second (first a-picture)))
          (add-offset offset (rest a-picture)))]))


;; (extract s-picture a-shapelist) extracts the Shape in a-shapelist whose
;;   ShapeID is the ShapeID of s-picture 
;; extract: SPicture ShapeList -> Shape
;; Example:
(check-expect (extract '((0 1) cone) summer-shapes)
              '(triangle cone (0 0) (20 0) (10 50) "Burlywood"))

(define (extract s-picture a-shapelist)
  (cond [(symbol=? (second s-picture) (shape-id (first a-shapelist)))
         (first a-shapelist)]
        [else (extract s-picture (rest a-shapelist))]))


;; (spicture->primitive s-picture a-shapelist) produces the list of PrimElements
;;   that generates s-picture given a-shapelist
;; spicture->primitive: SPicture ShapeList -> (listof PrimElement)
;;   requires: SPicture as defined in the data definition at the top
;;              of this document
;; Example:
(check-expect (spicture->primitive '((0 1) cone) summer-shapes)
              (list (make-prim-triangle (make-posn 0 1) (make-posn 20 1)
                                        (make-posn 10 51) "Burlywood")))

(define (spicture->primitive s-picture a-shapelist)
  (local [(define picture (extract s-picture a-shapelist))]
    
    (cond [(symbol=? 'component (shape-type picture))
           (picture->primitives
            (add-offset (first s-picture) (component-picture picture))
            a-shapelist)]
          [(symbol=? 'circle (shape-type picture))
           (cons (make-prim-circle (make-posn (point-x (first s-picture))
                                              (point-y (first s-picture)))
                                   (circle-radius picture)
                                   (circle-color picture))
                 empty)]
          [(symbol=? 'rectangle (first picture))
           (rect (make-posn (point-x (first s-picture))
                            (point-y (first s-picture)))
                 (make-posn (rect-width picture)
                            (rect-height picture))
                 (rect-color picture))]
          [(symbol=? 'triangle (first picture))
           (local [(define point1 (triangle-p1 picture))
                   (define point2 (triangle-p2 picture))
                   (define point3 (triangle-p3 picture))
                   (define offsetx (first (first s-picture)))
                   (define offsety (second (first s-picture)))]
             
             (cons (make-prim-triangle
                    (make-posn (+ (point-x point1) offsetx)
                               (+ (point-y point1) offsety))
                    (make-posn (+ (point-x point2) offsetx)
                               (+ (point-y point2) offsety))
                    (make-posn (+ (point-x point3) offsetx)
                               (+ (point-y point3) offsety))
                    (triangle-color picture)) empty))])))


;; (picture->primitive a-picture a-shapelist) produces the list of PrimElements
;;   that generates a-picture given a-shapelist
;; picture->primitive: Picture ShapeList -> (listof PrimElement)
;; Example:
(check-expect (picture->primitives '(((0 1) hello))
                                   '((rectangle hello 8 8 "Blue")))
              (list (make-prim-triangle (make-posn 0 1) (make-posn 8 1)
                                        (make-posn 8 9) "Blue")
                    (make-prim-triangle (make-posn 0 1) (make-posn 8 9)
                                        (make-posn 0 9) "Blue")))

(define (picture->primitives a-picture a-shapelist)
  (cond [(empty? a-picture) empty]
        [else (append (spicture->primitive (first a-picture) a-shapelist)
                      (picture->primitives (rest a-picture) a-shapelist))]))

;; Tests
(check-expect (picture->primitives '(((0 1) cone)
                                     ((2 2) top-scoop)) summer-shapes)
              (list (make-prim-triangle (make-posn 0 1) (make-posn 20 1)
                                        (make-posn 10 51) "Burlywood")
                    (make-prim-circle (make-posn 2 2) 10 "Pink")))
(check-expect (picture->primitives '(((20 30) ice-cream)) summer-shapes)
              (list (make-prim-triangle (make-posn 20 70) (make-posn 40 70)
                                        (make-posn 30 120) "Burlywood")
                    (make-prim-circle (make-posn 30 65) 10 "LightBlue")
                    (make-prim-circle (make-posn 30 55) 10 "Pink")))


;; ==== Question 3f ============================================================

;; (drawing->image bdrawing) renders bdrawing and produces a image
;; drawing->image: BundledDrawing -> Image

(define (drawing->image bdrawing)
  (render-image (make-posn (first bdrawing) (second bdrawing))
                (picture->primitives (third bdrawing) (fourth bdrawing))))

;;(drawing->image ice-cream-drawing)
