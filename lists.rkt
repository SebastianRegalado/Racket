;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 04, Question 3
;; *****************************************
;;


;; ==== Question 3a ============================================================

;; (absolutely-odd loi) produces the sum of the absolute values of the odd
;;   integers in loi
;; absolutely-odd: (listof Int) -> Nat
;; Example:
(check-expect (absolutely-odd empty) 0)

(define (absolutely-odd loi)
  (cond [(empty? loi) 0]
        [(odd? (first loi))
         (+ (abs (first loi)) (absolutely-odd (rest loi)))]
        [else (absolutely-odd (rest loi))]))

;; Tests
(check-expect (absolutely-odd (cons -1 (cons 2 (cons 3 empty)))) 4)
(check-expect (absolutely-odd (cons 2 (cons 4 (cons 6 empty)))) 0)
(check-expect (absolutely-odd (cons 10 (cons 9 (cons 8 empty)))) 9)


;; ==== Question 3b ============================================================

;; (spiraling? loi) produces a boolean value that indicates whether loi is a
;;    spiraling list
;; spiraling-list?: (listof Int) -> Bool
;; Example:
(check-expect (spiraling? empty) true)

(define (spiraling? loi)
  (cond [(empty? loi) true]
        [(empty? (rest loi)) true]
        [(and (< (* (first loi) (first (rest loi))) 0)
              (< (abs (first loi)) (abs (first (rest loi)))))
         (spiraling? (rest loi))]
        [else false]))

;; Tests
(check-expect (spiraling? (cons 0 empty)) true)
(check-expect (spiraling? (cons 1 (cons -10 (cons 100 empty)))) true)
(check-expect (spiraling? (cons -1 (cons 2 (cons -3 (cons 4 empty))))) true)
(check-expect (spiraling? (cons 99 (cons -100 (cons 100 empty)))) false)
(check-expect (spiraling? (cons 0 (cons -10 (cons 100 empty)))) false)

;; ==== Question 3c ============================================================

;; (length-list lst) produces the lenght of lst
;; length-list: (listof Any) -> Nat
;; Example:
(check-expect (length-list (cons 'a (cons 'b empty))) 2)

(define (length-list lst)
  (cond [(empty? lst) 0]
        [else (+ 1 (length-list (rest lst)))]))

;; (product-list lon) produces the product of all the elements in lon
;; length-list: (listof Num) -> Num
;; Example:
(check-expect (product-list (cons 4 (cons 5 empty))) 20)

(define (product-list lon)
  (cond [(empty? lon) 1]
        [else (* (first lon) (product-list (rest lon)))]))

;; (geometric-mean lon) produces the geometric mean of all the elements in lon
;; geometric-mean: (listof Num) -> Num
;; requires: lon to be formed only by positive numbers
;;           lon to be non-empty

(define (geometric-mean lon)
  (expt (product-list lon) (/ 1 (length-list lon))))

(check-within (geometric-mean (cons 9 (cons 0.5 (cons 6 empty)))) 3 0.0001)
(check-within (geometric-mean (cons 7 (cons 8 (cons 6 empty)))) 6.952053 0.0001)
(check-within (geometric-mean (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              2.2133 0.0001)