;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname park-rentals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 04, Question 2
;; *****************************************
;;

;;; Structure and data definitions

(define-struct bicycle (make model serial-number))
;; A Bicycle is a (make-bicycle Str Str Nat)

(define-struct boat (type serial-number))
;; A Boat is a (make-boat Sym Nat)
;; requires: type is 'paddle-boat or 'canoe

(define-struct horse (name serial-number capacity stamina))
;; A Horse is a (make-horse Str Nat Nat Num)
;; requires: capacity > 0, stamina > 0

;; A Rental is (anyof Bicycle Boat Horse)


;; ==== Question 2a ============================================================

;; rental-template: Rental -> Any
(define (rental-template rental) 
  (... (cond [(bicycle? rental)
              (... (bicycle-make rental) ...
                   (bicycle-model rental) ...
                   (bicycle-serial-number rental) ...)]
             [(boat? rental)
              (... (boat-type rental) ...
                   (boat-serial-number rental) ...)]
             [(horse? rental)
              (... (horse-name rental) ...
                   (horse-serial-number rental) ...
                   (horse-capacity rental) ...
                   (horse-stamina rental) ...)])))


;; ==== Question 2b ============================================================

;; (rental-id rental) produces the serial number associated with rental
;; rental-id: Rental -> Nat
;; Example:
(check-expect (rental-id (make-bicycle "Cannondale" "Jekyll" 321)) 321)

(define (rental-id rental)
  (cond [(bicycle? rental) (bicycle-serial-number rental)]
        [(boat? rental) (boat-serial-number rental)]
        [(horse? rental) (horse-serial-number rental)]))

;; Tests
(check-expect (rental-id (make-horse "Stomper" 82 4 1.3)) 82)
(check-expect (rental-id (make-boat 'canoe 763)) 763)

;; ==== Question 2c ============================================================

(define bic-capacity 1)
(define canoe-max-capacity 2)
(define paddle-max-capacity 3)

;; (rental-ok? rental num-renters hours) produces true if rental has sufficient
;;   capacity for the number of renters and the duration of the rental is valid
;;   otherwise false
;; rental-ok?: Rental Nat Num -> Bool
;; requires: hours >= 0
;; Example:
(check-expect (rental-ok? (make-bicycle "Cannondale" "Jekyll" 321) 1 2.3) true)

(define (rental-ok? rental num-renters hours)
  (cond [(bicycle? rental) (= num-renters bic-capacity)]
        [(boat? rental)
         (cond [(symbol=? (boat-type rental) 'canoe)
                (<= num-renters canoe-max-capacity)]
               [(symbol=? (boat-type rental) 'paddle-boat)
                (<= num-renters paddle-max-capacity)])]
        [(horse? rental)
         (and (<= num-renters (horse-capacity rental))
              (<= hours (horse-stamina rental)))]))

;; Tests
(check-expect (rental-ok? (make-bicycle "Cannondale" "Jekyll" 321) 4 2.3) false)
(check-expect (rental-ok? (make-boat 'canoe 456) 1 2.3) true)
(check-expect (rental-ok? (make-boat 'canoe 456) 2 2.3) true)
(check-expect (rental-ok? (make-boat 'canoe 456) 4 2.3) false)
(check-expect (rental-ok? (make-boat 'paddle-boat 334) 2 2.3) true)
(check-expect (rental-ok? (make-boat 'paddle-boat 334) 3 2.3) true)
(check-expect (rental-ok? (make-boat 'paddle-boat 334) 4 2.3) false)
(check-expect (rental-ok? (make-horse "Stomper" 82 4 1.3) 5 1) false)
(check-expect (rental-ok? (make-horse "Stomper" 82 4 1.3) 3 1.5) false)
(check-expect (rental-ok? (make-horse "Stomper" 82 4 1.3) 3 1) true)

;; ==== Question 2d ============================================================

(define bic-cost 20)
(define boat-cost 30)
(define horse-base-cost 30)
(define add-horse-price 10)
(define max-renters-base-price 3)

;; (rental-price rental num-renters hours) produces the cost
;;   associated with rental given num-renters and hours
;; rental-price: Rental Nat Num -> Nat
;; requires: rental, num-renters, and hours, such that
;;           (rental-ok? rental num-renters hours) is true
;;           hours >=0
;; Example:
(check-expect (rental-price (make-bicycle "Cannondale" "Jekyll" 321) 1 2.3) 60)

(define (rental-price rental num-renters hours)
  (cond [(bicycle? rental) (* bic-cost (ceiling hours))]
        [(boat? rental) (* boat-cost (ceiling hours))]
        [else (+ (* (ceiling (* 2 hours)) horse-base-cost)
                 (cond [(> num-renters max-renters-base-price)
                        (* add-horse-price
                           (ceiling (* 2 hours))
                           (- num-renters max-renters-base-price))]
                       [else 0]))]))

;; Tests
(check-expect (rental-price (make-boat 'canoe 456) 4 2.3) 90)
(check-expect (rental-price (make-boat 'paddle-boat 334) 2 2.3) 90)
(check-expect (rental-price (make-horse "Stomper" 82 4 1.3) 2 1.3) 90)
(check-expect (rental-price (make-horse "Stomper" 82 8 3) 5 1.75) 200)