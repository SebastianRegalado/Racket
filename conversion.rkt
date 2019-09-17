;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp")) #t)))
;;
;; ***************************************************
;; Sebastian Regalado (20759473)
;; CS 135 Winter 2019
;; Assignment 01, Problem 3
;; ***************************************************
;;


;;(a)
;;Useful converters
(define kms-in-a-mile 1.609344)
(define 100kms-in-a-mile (/ kms-in-a-mile 100))
(define gallons-in-a-liter (/ 1 3.785411784))
(define mpg-to-100kmspl-ratio
  (* 100kms-in-a-mile gallons-in-a-liter)) ;up *******


;; (mpg->lp100km a-mpg-fe) produces a liters-per-100km fuel  
;;   efficiency equivalent to a-mpg-fe
;; mpg->lp100km: Num -> Num
;; Example: ******** 

(define (mpg->lp100km a-mpg-fe)
  (/ 1 (* a-mpg-fe mpg-to-100kmspl-ratio)))


;; ***************************************************


;;(b)
;;Useful converters
(define yards-in-a-rod 5.5)
(define yards-in-a-mile 1760)
(define rods-in-a-mile (/ yards-in-a-mile yards-in-a-rod))
(define gallons-in-a-hogshead 79)


;; (mpg->rph a-mpg-fe) produces a rods-per-hogshead fuel  
;;   efficiency equivalent to a a-mpg-fe
;; mpg->rph: Num -> Num
;; Example: ********

(define (mpg->rph a-mpg-fe)
  (* a-mpg-fe rods-in-a-mile gallons-in-a-hogshead))
