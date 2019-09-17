;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quaternion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 03, Bonus Question
;; *****************************************
;;

(define-struct quaternion (cc ic jc kc))

;; (product-cc a b quaternion1 quaternion2) produces the constant coeficient
;;   in the product of quaternion1 and quaternion2
;; product-cc: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (product-cc 3 4 (make-quaternion 1 2 3 4)
                          (make-quaternion 1 1 3 1)) -5)

(define (product-cc a b quaternion1 quaternion2)
  (+ (* (quaternion-cc quaternion1) (quaternion-cc quaternion2))
     (* (quaternion-ic quaternion1) (quaternion-ic quaternion2) a)
     (* (quaternion-jc quaternion1) (quaternion-jc quaternion2) b)
     (* (quaternion-kc quaternion1) (quaternion-kc quaternion2) a b -1)))

;; (product-ic a b quaternion1 quaternion2) produces the coeficient of i
;;   in the product of quaternion1 and quaternion2
;; product-ic: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (product-ic 3 4 (make-quaternion 1 2 3 4)
                          (make-quaternion 1 1 3 1)) 39)

(define (product-ic a b quaternion1 quaternion2)
  (+ (* (quaternion-ic quaternion1) (quaternion-cc quaternion2))
     (* (quaternion-cc quaternion1) (quaternion-ic quaternion2))
     (* (quaternion-jc quaternion1) (quaternion-kc quaternion2) b -1)
     (* (quaternion-kc quaternion1) (quaternion-jc quaternion2) b)))

;; (product-jc a b quaternion1 quaternion2) produces the coeficient of j
;;   in the product of quaternion1 and quaternion2
;; product-jc: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (product-jc 3 4 (make-quaternion 1 2 3 4)
                          (make-quaternion 1 1 3 1)) 0)

(define (product-jc a b quaternion1 quaternion2)
  (+ (* (quaternion-jc quaternion1) (quaternion-cc quaternion2))
     (* (quaternion-cc quaternion1) (quaternion-jc quaternion2))
     (* (quaternion-ic quaternion1) (quaternion-kc quaternion2) a)
     (* (quaternion-kc quaternion1) (quaternion-ic quaternion2) a -1)))

;; (product-kc a b quaternion1 quaternion2) produces the coeficient of k
;;   in the product of quaternion1 and quaternion2
;; product-kc: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (product-kc 3 4 (make-quaternion 1 2 3 4)
                          (make-quaternion 1 1 3 1)) 8)

(define (product-kc a b quaternion1 quaternion2)
  (+ (* (quaternion-kc quaternion1) (quaternion-cc quaternion2))
     (* (quaternion-cc quaternion1) (quaternion-kc quaternion2))
     (* (quaternion-ic quaternion1) (quaternion-jc quaternion2))
     (* (quaternion-jc quaternion1) (quaternion-ic quaternion2) -1)))

;; (quat-mult a b q1 q2) produces the product the of the quaternions
;;   q1 and q2
;; quat-mult: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (quat-mult 3 4 (make-quaternion 1 2 3 4)
                         (make-quaternion 1 1 3 1))
              (make-quaternion -5 39 0 8))

(define (quat-mult a b q1 q2)
  (make-quaternion (product-cc a b q1 q2)
                   (product-ic a b q1 q2)
                   (product-jc a b q1 q2)
                   (product-kc a b q1 q2)))

;; Tests
(check-expect (quat-mult 3 4 (make-quaternion 1 2 3 4)
                         (make-quaternion 1 1 3 1))
              (make-quaternion -5 39 0 8))
(check-expect (quat-mult 0 3 (make-quaternion 2 2 2 2)
                         (make-quaternion 1 1 3 1))
              (make-quaternion 20 16 8 8))
(check-expect (quat-mult 2 0 (make-quaternion 1 2 3 4)
                         (make-quaternion 0 0 3 2))
              (make-quaternion 0 0 11 8))
