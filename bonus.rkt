;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 08, Bonus question
;; *****************************************
;;


;; ==== Part a =================================================================

;; Useful constant functions
(define (p x) (+ x 9))
(define (q x) (* x x))
(define (r x y) (+ (* x x) y))

;; (my-compose f g) produces a function that when applied to an argument x gives
;;   the same result as if g is applied to x and then f is applied to the result
;; my-compose: (Y -> Z) (X -> Y) -> (X -> Z)

(define (my-compose f g)
  (lambda (x) (f (g x))))

(check-expect ((my-compose p q) 1) 10)

;; ==== Part b =================================================================

;; (curry f) produces a one-argument function that when applied to an argument x
;;   produces another function that, if applied to an argument y, gives the same
;;   result as if f had been applied to the two arguments x and y
;; curry: (X Y -> Z) -> (X -> (Y -> Z))

(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

(check-expect (((curry r) 1) 3) 4)
(check-expect (((curry r) 2) 3) 7)

;; ==== Part c =================================================================

;; (uncurry f) for any two-argument function f , (uncurry (curry f)) is
;;   functionally equivalent to f
;; uncurry: (X -> (Y -> Z)) -> (X Y -> Z)

(define (uncurry f)
  (lambda (x y) ((f x) y)))

(check-expect ((uncurry (curry r)) 1 3) 4)

;; ==== Part d =================================================================

;; (eat-apples los) remove all the symbols in los that are equal to 'apple
;; eat-apples: (listof Sym) -> (listof Sym)

(define (eat-apples los)
  (filter (my-compose not ((curry symbol=?) 'apple)) los))

(check-expect (eat-apples '(apple fbd dfb dfb bsafsdag gfsd apple))
              '(fbd dfb dfb bsafsdag gfsd))

;; ==== Part e =================================================================

;; (my-map f lst) produces the same result as (map f lst)
;; my-map: (X -> Y) (listof X) -> (listof Y)

(define (my-map f lst)
  (foldr (uncurry (my-compose (curry cons) f)) empty lst))

(check-expect (my-map p '(1 2 3)) '(10 11 12))
