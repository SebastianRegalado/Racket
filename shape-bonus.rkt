;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname shape-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 07, Q3g (bonus)
;; *****************************************
;;

(require "a07drawinglib.rkt")

(define (rect top-left dimensions color)
  (list (make-prim-triangle
         top-left
         (make-posn (+ (posn-x dimensions) (posn-x top-left)) (posn-y top-left))
         (make-posn (+ (posn-x dimensions) (posn-x top-left))
                    (+ (posn-y dimensions) (posn-y top-left)))
         color)
        (make-prim-triangle
         top-left
         (make-posn (+ (posn-x dimensions) (posn-x top-left))
                    (+ (posn-y dimensions) (posn-y top-left)))
         (make-posn (posn-x top-left) (+ (posn-y dimensions) (posn-y top-left)))
         color)))

(define (picture->primitives a-picture a-shapelist)
  (cond [(empty? a-picture) empty]
        [else (append (spicture->primitive (first a-picture) a-shapelist)
                      (picture->primitives (rest a-picture) a-shapelist))]))

(define (add-offset offset a-picture)
  (cond [(empty? a-picture) empty]
        [else (cons
               (list
                (list (+ (first offset) (first (first (first a-picture))))
                      (+ (second offset) (second (first (first a-picture)))))
                (second (first a-picture)))
               (add-offset offset (rest a-picture)))]))

(define (extract s-picture a-shapelist)
  (cond [(symbol=? (second s-picture) (second (first a-shapelist)))
         (first a-shapelist)]
        [else (extract s-picture (rest a-shapelist))]))

(define (spicture->primitive s-picture a-shapelist)
  (local [(define picture (extract s-picture a-shapelist))]
    (cond [(symbol=? 'component (first picture))
           (picture->primitives
            (add-offset (first s-picture) (third picture))
            a-shapelist)]
          [(symbol=? 'circle (first picture))
           (cons (make-prim-circle (make-posn (first (first s-picture))
                                              (second (first s-picture)))
                                   (third picture)
                                   (fourth picture))
                 empty)]
          [(symbol=? 'rectangle (first picture))
           (rect (make-posn (first (first s-picture))
                            (second (first s-picture)))
                 (make-posn (third picture)
                            (fourth picture))
                 (fifth picture))]
          [(symbol=? 'triangle (first picture))
           (cons (make-prim-triangle
                  (make-posn (+ (first (third picture))
                                (first (first s-picture)))
                             (+ (second (third picture))
                                (second (first s-picture))))
                  (make-posn (+ (first (fourth picture))
                                (first (first s-picture)))
                             (+ (second (fourth picture))
                                (second (first s-picture))))
                  (make-posn (+ (first (fifth picture))
                                (first (first s-picture)))
                             (+ (second (fifth picture))
                                (second (first s-picture))))
                  (sixth picture)) empty)])))

(define (drawing->image bdrawing)
  (render-image (make-posn (first bdrawing) (second bdrawing))
                (picture->primitives (third bdrawing) (fourth bdrawing))))

(define fun-shapes '((circle circulin-1 50 "Red")
                     (circle circulin-2 55 "Green")
                     (circle circulin-3 60 "Red")
                     (circle circulin-4 65 "Green")
                     (circle circulin-5 70 "Red")
                     (circle circulin-6 75 "Green")
                     (circle circulin-7 80 "Red")
                     (circle circulin-8 85 "Green")
                     (circle circulin-9 90 "Red")
                     (circle circulin-10 95 "Green")
                     (circle circulin-11 100 "Red")
                     (circle circulin-12 105 "Green")
                     (triangle tri-force-1 (150 130) (125 170)
                               (175 170) "Yellow")
                     (triangle tri-force-2 (5 10) (0 15) (10 15) "Yellow")
                     (triangle tri-force-3 (3 3) (2 7) (8 4) "Pink")
                     (triangle tri-force-4 (2 3) (6 9) (56 4) "Snow")
                     (rectangle rect-1 250 250 "Orange")
                     (component circulon
                                (((0 0) circulin-1)
                                 ((0 0) circulin-2)
                                 ((0 0) circulin-3)
                                 ((0 0) circulin-4)
                                 ((0 0) circulin-5)
                                 ((0 0) circulin-6)
                                 ((0 0) circulin-7)
                                 ((0 0) circulin-8)
                                 ((0 0) circulin-9)
                                 ((0 0) circulin-10)
                                 ((0 0) circulin-11)
                                 ((0 0) circulin-12)))
                     (component triforce
                                (((0 2) tri-force-1)
                                 ((4 3) tri-force-2)
                                 ((5 8) tri-force-3)))
                     (component figurilla
                                (((4 5) triforce)
                                 ((6 0) rect-1)))
                     (component picturilla
                                (((2 2) triforce)
                                 ((1 1) figurilla)
                                 ((3 0) tri-force-4)
                                 ((5 5) circulin-2)))))

(define fun-pic '(((0 0) tri-force-1)
                      ((150 150) circulon)
                      ((25 25) rect-1)))

(define shape-drawing (list 300 300 fun-pic fun-shapes))