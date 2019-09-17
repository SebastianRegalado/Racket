;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 03, Question 4
;; *****************************************
;;


;; ==== Question 4a ========================

;; (posn-mult posn1 posn2) produces the multiplication of two Posns
;;   posn1 and posn2
;; posn-mult: Posn Posn -> Posn
;; Example:
(check-expect (posn-mult (make-posn 4 3) (make-posn 3 4)) (make-posn 0 25))

(define (posn-mult posn1 posn2)
  (make-posn (- (* (posn-x posn1) (posn-x posn2))
                (* (posn-y posn1) (posn-y posn2)))
             (+ (* (posn-x posn1) (posn-y posn2))
                (* (posn-x posn2) (posn-y posn1)))))

;; Tests
(check-expect (posn-mult (make-posn 0 3) (make-posn 0 4)) (make-posn -12 0))
(check-expect (posn-mult (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))

;; ==== Question 4b ========================

;; (posn-div posn1 posn2) produces the divisions of two Posns
;;   posn1 and posn2
;; posn-mult: Posn Posn -> Posn
;; requires: posn2 not equal to (make-posn 0 0)
;; Example:
(check-expect (posn-div (make-posn 4 3) (make-posn 4 3)) (make-posn 1 0))

(define (posn-div posn1 posn2)
  (make-posn (/ (+ (* (posn-x posn1) (posn-x posn2))
                   (* (posn-y posn1) (posn-y posn2)))
                (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))
             (/ (- (* (posn-y posn1) (posn-x posn2))
                   (* (posn-x posn1) (posn-y posn2)))
                (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))))

;; Tests
(check-expect (posn-div (make-posn 14 8) (make-posn 7 4)) (make-posn 2 0))
(check-expect (posn-div (make-posn 0 0) (make-posn 31 4)) (make-posn 0 0))

;; ==== Question 4c ========================

;; (posn-reciprocal posn) produces the reciprocal of posn
;; posn-reciprocal: Posn -> Posn
;; requires: posn not equal to (make-posn 0 0)
;; Example:
(check-expect (posn-reciprocal (make-posn 4 3)) (make-posn (/ 4 25) (/ -3 25)))

(define (posn-reciprocal posn)
  (make-posn (/ (posn-x posn)
                (+ (sqr (posn-x posn)) (sqr (posn-y posn))))
             (/ (* -1 (posn-y posn))
                (+ (sqr (posn-x posn)) (sqr (posn-y posn))))))

;; Tests
(check-expect (posn-reciprocal (make-posn 0 4)) (make-posn 0 -0.25))
(check-expect (posn-reciprocal (make-posn 5 0)) (make-posn 0.2 0))