;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname subsets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment A9, Q4 (bonus)
;; *****************************************
;;




;; ==== Question 4a=============================================================

;; (subsets1 lon) produces a list with all the subsets of lon
;; subsets1: (listof Num) -> (listof (listof Num))
;; Example:
(check-expect (subsets1 '(1 2))
              (list empty (list 2) (list 1) (list 1 2)))

(define (subsets1 lon)
  (foldr (lambda (x y) (append y (map (lambda (z) (cons x z)) y)))
         (list empty) lon))

(check-expect (subsets1 '(1 2 3))
              (list empty (list 3) (list 2) (list 2 3) (list 1)
                    (list 1 3) (list 1 2) (list 1 2 3)))


;; ==== Question 4b=============================================================

;; (subsets2 lon) produces a list with all the subsets of lon
;; subsets2: (listof Num) -> (listof (listof Num))
;; Example:
(check-expect (subsets2 '(1 2))
              (list empty (list 2) (list 1) (list 1 2)))

(define (subsets2 lon)
  (foldr (lambda (x y) (append y (map (lambda (z) (cons x z)) y)))
         (list empty) lon))

(check-expect (subsets2 '(1 2 3))
              (list empty (list 3) (list 2) (list 2 3) (list 1)
                    (list 1 3) (list 1 2) (list 1 2 3)))

;; ==== Question 4c=============================================================

