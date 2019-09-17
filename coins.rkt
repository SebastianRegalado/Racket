;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname coins) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment A9, Q2
;; *****************************************
;;



;; === Constants used in examples===============================================


(define berg-coins '((kaf 2) (riwu 4) (teyne 10) (spok 16) (dekaf 20)))


;; other useful symbols
;; 'dime 'nickel 'quarter 'loonie 'toonie 

;; strictly speaking the assignment pdf also uses 'button but only as an example
;; "does not match" so you shouldn't be too concerned about spelling!

;; a Association List (AL) is a (listof (list Sym Num))
;; requires: all the symbols in Al are unique

;; ==== Question 2a=============================================================

;; (coin-total los) produces the total value of the all the coins in los
;; coin-total: (listof Sym) -> Num
;; Example: 
(check-expect (coin-total '(nickel dime dime quarter button)) 0.50)

(define (coin-total los)
  (foldr (lambda (sym rror)
           (+ rror (cond [(symbol=? sym 'nickel) 0.05]
                         [(symbol=? sym 'dime) 0.1]
                         [(symbol=? sym 'quarter) 0.25]
                         [(symbol=? sym 'loonie) 1]
                         [(symbol=? sym 'toonie) 2]
                         [else 0])))
         0 los))

;; Tests
(check-expect (coin-total '(nickel dime button)) 0.15)
(check-expect (coin-total '(nickel loonie dime button bill)) 1.15)
(check-expect (coin-total '(nickel dime button toonie moneda)) 2.15)
(check-expect (coin-total '(bye hola button)) 0)


;; ==== Question 2b=============================================================

;; (coin-counts los) produces an association list where the keys are Canadian
;;   coins, and the values are the number of occurrences of that coin in los
;; coin-counts: (listof Sym) -> AL
;; Example:
(check-expect (coin-counts '(nickel dime dime quarter button))
              '((nickel 1) (dime 2) (quarter 1) (loonie 0) (toonie 0)))

(define (coin-counts los)
  (map (lambda (coin) (list coin
                            (foldr (lambda (sym rror)
                                     (+ rror (cond [(symbol=? coin sym) 1]
                                                   [else 0]))) 0 los)))
       '(nickel dime quarter loonie toonie)))

;; Tests
(check-expect (coin-counts '(nickel button dime dime dime quarter
                                    quarter button))
              '((nickel 1) (dime 3) (quarter 2) (loonie 0) (toonie 0)))
(check-expect (coin-counts '(hola button dame hi pineapple))
              '((nickel 0) (dime 0) (quarter 0) (loonie 0) (toonie 0)))


;; ==== Question 2c=============================================================

;; (foreign-coin-total al los) produces the total value of all the coins in los
;;   according to the association list al
;; foreign-coint-total: AL (listof Sym) -> Num
;; Example:
(check-expect (foreign-coin-total berg-coins '(spok riwu spok)) 36)

(define (foreign-coin-total al los)
  (foldr (lambda (pair rror)
           (+ rror (foldr (lambda (sym rror2)
                            (+ rror2
                               (cond [(symbol=? (first pair) sym)
                                      (second pair)]
                                     [else 0]))) 0 los)))
         0 al))

;; Tests
(check-expect (foreign-coin-total berg-coins '(riwu riwu teyne teyne spok)) 44)
(check-expect (foreign-coin-total berg-coins '(dekaf spok teyne riwu)) 50)
(check-expect (foreign-coin-total berg-coins empty) 0)


;; ==== Question 2d=============================================================

;; (make-coin-totaler al) produces a function the consumes a list of symbols and
;;    produces their total value according to the association list al
;; make-coin-totaler: AL -> ((listof Sym) -> Num)
;; Example:
(check-expect ((make-coin-totaler berg-coins) '(spok riwu spok)) 36)

(define (make-coin-totaler al)
  (lambda (los) (foldr (lambda (pair rror)
                         (+ rror (foldr (lambda (sym rror2)
                                          (+ rror2
                                             (cond [(symbol=? (first pair) sym)
                                                    (second pair)]
                                                   [else 0]))) 0 los)))
                       0 al)))

;; Tests
(check-expect ((make-coin-totaler empty) '(spok riwu spok)) 0)
(check-expect ((make-coin-totaler '((spok 1) (hola 2))) '(spok riwu spok)) 2)
(check-expect ((make-coin-totaler '((hola 2))) '(spok riwu spok)) 0)