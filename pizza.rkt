;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 02, Question 3
;; *****************************************
;;

(define price-small 5)
(define price-medium 7)
(define price-large 8.5)
(define price-elarge 10)
(define price-stop 0.75)
(define price-ptop 1.25)

;; Size is (anyof 'small 'medium 'large 'extra-large)
;; Coupoun is (anyof 'two-off 'upsize 'three-top 'premium 'solo
;;                   'party 'loaded 'none)

;; (normal-price size stan-top prem-top) produces the price of a 
;;  pizza based on size, stan-top and prem-top
;; normal-price: Size Nat Nat -> Num
;; Example:
(check-expect (normal-price 'small 0 0) 5)

(define (normal-price size stan-top prem-top)
  (+ (cond
       [(symbol=? size 'small) price-small]
       [(symbol=? size 'medium) price-medium]
       [(symbol=? size 'large) price-large]
       [else price-elarge])
     (* stan-top price-stop)
     (* prem-top price-ptop)))


;; (pizza-price size stan-top prem-top coupon) produces the price of a 
;;  pizza based on given size, stan-top, prem-top and coupon
;; pizza-price: Size Nat Nat Coupon -> Num
;; Examples:
(check-expect (pizza-price 'large 1 0 'none) 9.25)
(check-expect (pizza-price 'medium 0 1 'upsize) 6.25)
(check-expect (pizza-price 'large 1 0 'upsize) 7.75)
(check-expect (pizza-price 'small 3 1 'three-top) 6.25)
(check-expect (pizza-price 'large 2 0 'three-top) 8.5)

(define (pizza-price size stan-top prem-top coupon)
  (cond
    [(symbol=? coupon 'none) (normal-price size stan-top prem-top)]
    [(symbol=? coupon 'two-off) (- (normal-price size stan-top prem-top) 2)]
    [(symbol=? coupon 'upsize) 
     (- (normal-price size stan-top prem-top)
        (cond [(symbol=? size 'small) 0]
              [(symbol=? size 'medium) 2]
              [else 1.5]))]
    [(symbol=? coupon 'three-top) (- (normal-price size stan-top prem-top)
                                     (* (min 3 stan-top) price-stop))]
    [(symbol=? coupon 'premium) (- (normal-price size stan-top prem-top)
                                   (* prem-top (- price-ptop price-stop)))]
    [(and (symbol=? coupon 'solo)
          (symbol=? size 'small)
          (= stan-top 0)
          (= prem-top 2)) 6]
    [(and (symbol=? coupon 'party)
          (symbol=? size 'extra-large)
          (= stan-top 3)
          (= prem-top 0)) 11]
    [(and (symbol=? coupon 'loaded) (symbol=? size 'extra-large)) 15]
    [else (normal-price size stan-top prem-top)]))

;;Tests
(check-expect (pizza-price 'medium 1 0 'two-off) 5.75)
(check-expect (pizza-price 'small 1 0 'upsize) 5.75)
(check-expect (pizza-price 'extra-large 0 1 'upsize) 9.75)
(check-expect (pizza-price 'medium 5 1 'three-top) 9.75)
(check-expect (pizza-price 'small 1 0 'premium) 5.75)
(check-expect (pizza-price 'medium 1 3 'premium) 10)
(check-expect (pizza-price 'extra-large 1 0 'solo) 10.75)
(check-expect (pizza-price 'small 1 2 'solo) 8.25)
(check-expect (pizza-price 'small 0 3 'solo) 8.75)
(check-expect (pizza-price 'small 0 2 'solo) 6)
(check-expect (pizza-price 'large 1 0 'party) 9.25)
(check-expect (pizza-price 'extra-large 1 2 'party) 13.25)
(check-expect (pizza-price 'extra-large 3 1 'party) 13.50)
(check-expect (pizza-price 'extra-large 3 0 'party) 11)
(check-expect (pizza-price 'medium 1 1 'loaded) 9)
(check-expect (pizza-price 'extra-large 1 0 'loaded) 15)

