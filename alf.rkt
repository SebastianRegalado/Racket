;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 08, Question 3
;; *****************************************
;;



;; ==== Question 3a ============================================================

;; (occurrences lon n) produces the number of times that n occurs in lon
;; ocurrences: (listof Num) -> Nat
;; Example
(check-expect (occurrences '(1 2) 0) 0)

(define (occurrences lon n)
  (length (filter (lambda (x) (= n x)) lon)))

;; Tests
(check-expect (occurrences '() 3) 0)
(check-expect (occurrences '(1 2 1 2 2 3 1) 2) 3)
(check-expect (occurrences '(1 2 5 5 3 1) 5) 2)


;; ==== Question 3b ============================================================

;; (absolutely-odd loi) produces the sum of the absolute values of the odd
;;   integers in loi
;; absolutely-odd: (listof Int) -> Nat
;; Example:
(check-expect (absolutely-odd '(2 6 8 4)) 0)

(define (absolutely-odd loi)
  (foldr + 0 (map abs (filter odd? loi))))

;; Tests
(check-expect (absolutely-odd '()) 0)
(check-expect (absolutely-odd '(1 -5 4 6 5)) 11)
(check-expect (absolutely-odd '(-9 4 5 2 -3)) 17)


;; ==== Question 3c ============================================================

;; (zip lst1 lst2) produces a list of pairs where the ith pair contains the ith
;;   element of lst1 followed by the ith element of lst2
;; zip: (listof Any) (listof Any) -> (listof (list Any Ant))
;; requires: lst1 and lst2 have the same length
;; Example:
(check-expect (zip '() '()) '())

(define (zip lst1 lst2)
  (map (lambda (x y) (list x y)) lst1 lst2))

;; Tests
(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))
(check-expect (zip '("hola") '(2)) '(("hola" 2)))

;; ==== Question 3d ============================================================

;; (unzip lop) produces a list of two lists where the first list contains the 
;;   first element of each pair in lop, and the second list contains the second 
;;   element of each pair in lop, in the original order
;; unzip: (listof (list Any Any))
;; Example:
(check-expect (unzip '()) '(()()))

(define (unzip lop)
  (list (map first lop)
        (map second lop)))

;; Tests
(check-expect (unzip '(("hola" 2))) '(("hola") (2)))
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))


;; ==== Question 3e ============================================================

;; (dedup lon) produces a list of numbers with only the first occurrence of each
;;   element of the original list
;; dedup: (listof Num) -> (listof Num)
;; Example:
(check-expect (dedup '()) '())

(define (dedup lon)
  (cond [(empty? lon) empty]
        [else (foldr (lambda (x y)
                       (cons x (filter (lambda (z) (not (= x z))) y)))
                     lon lon)]))

;; Tests
(check-expect (dedup '(1 2 1 3 3 2 4)) '(1 2 3 4))
(check-expect (dedup '( 1 6 3 2 5 4 8 5 1 2 3 6 4 2)) '(1 6 3 2 5 4 8))


;; ==== Question 3f ============================================================

;; (subsequence lst from to) produces the subsequence from lst that begins at
;;   index from and ends just before index to
;; subsequence: (listof Num) Nat Nat -> (listof Num)
;; Example:
(check-expect (subsequence '() 0 8) '())

(define (subsequence lst from to)
  (first (unzip (filter (lambda (x) (and (< from (second x))
                                         (<= (second x) to)))
                        (zip lst (build-list (length lst) add1))))))

;; Tests
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence '(a b c d e f g) 1 1) '())
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))
(check-expect (subsequence '(a b c d) 30 400) '())

