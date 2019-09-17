;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname geometry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 03, Question 2
;; *****************************************
;;

;; (euclidean-distance p1 p2) produces the euclidean distance  
;;   between p1 and p2
;; euclidean-distance: Posn Posn -> Num
;; Example:
(check-expect (euclidean-distance (make-posn 3 4) (make-posn 0 0)) 5)

(define (euclidean-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; (semi-perimeter p1 p2 p3) produces the semi-perimeter of the triangle  
;;   formed by p1, p2, and p3
;; semi-perimeter: Posn Posn Posn -> Num
;; Example:
(check-expect (semi-perimeter (make-posn -3 0) (make-posn 3 0) (make-posn 0 4))
              8)

(define (semi-perimeter p1 p2 p3)
  (/ (+ (euclidean-distance p1 p2)
        (euclidean-distance p1 p3)
        (euclidean-distance p2 p3))
     2))

;; (triangle-area p1 p2 p3) produces the area of the triangle  
;;   formed by p1, p2, and p3
;; triangle-area: Posn Posn Posn -> Num
;; Example:
(check-within (triangle-area (make-posn -6 0) (make-posn 6 0) (make-posn 0 8))
              48 0.00001)

(define (triangle-area p1 p2 p3)
  (sqrt (* (semi-perimeter p1 p2 p3)
           (- (semi-perimeter p1 p2 p3) (euclidean-distance p1 p2))
           (- (semi-perimeter p1 p2 p3) (euclidean-distance p1 p3))
           (- (semi-perimeter p1 p2 p3) (euclidean-distance p2 p3)))))

;; Tests
(check-within (triangle-area (make-posn 0 0) (make-posn 3 3) (make-posn 5 4))
              1.5 0.00001)
(check-within (triangle-area (make-posn 0 0) (make-posn 2 2) (make-posn 4 4))
              0 0.00001)
(check-within (triangle-area (make-posn 12 5) (make-posn 2 6) (make-posn 2 6))
              0 0.00001)
(check-within (triangle-area (make-posn 13 7) (make-posn 13 7) (make-posn 13 7))
              0 0.00001)

