;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname codes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 05, Question 2
;; *****************************************
;;


;; A Decryptor is a (list Nat Nat Nat)


;; ==== Question 2a ============================================================


;; (get-element lst n) produces the Char in the list lst at the postion n if it
;;   exists, and #\* otherwise
;; get-element: (listof Char) Nat -> Char 
;; Example:
(check-expect (get-element (string->list "abcdefg") 0) #\a)

(define (get-element lst n)
  (cond [(empty? lst) #\*]
        [(= n 0) (first lst)]
        [else (get-element (rest lst) (sub1 n))]))


;; (get-element string n) produces the character in string at the position n 
;; get-char: Str Nat -> Char
;; Example:
(check-expect (get-char "abcdefg" 0) #\a)

(define (get-char string n)
  (get-element (string->list string) n))

;; Tests
(check-expect (get-char "abcdefg" 3) #\d)
(check-expect (get-char "abcdefg" 20) #\*)


;; ==== Question 2b ============================================================


;; (coded-3-char string dec) produces a string containing the 3-character
;;   “secret message” hidden in the string using the decryptor dec
;; coded-3-char: Str Decryptor -> Str
;; Example:
(check-expect (coded-3-char "abcdefg" (list 0 0 0)) "abc")

(define (coded-3-char string dec)
  (list->string (list (get-char string (first dec))
                      (get-char string (+ (first dec) (second dec) 1))
                      (get-char string (+ (first dec) (second dec)
                                          (third dec) 2)))))
;; Tests
(check-expect (coded-3-char "abcdefg" (list 2 1 0)) "cef")
(check-expect (coded-3-char "abcdefg" (list 2 2 3)) "cf*")
(check-expect (coded-3-char "abcdefg" (list 2 10 0)) "c**")
(check-expect (coded-3-char "abcdefg" (list 10 10 0)) "***")


;; ==== Question 2c ============================================================


;; (ordered-sub-list? lst1 lst2) produces true if lst2 is ordered sub-list of
;;   of lst1, and false otherwise 
;; ordered-sub-list?: (listof Any) (listof Any) -> Bool
;; Example:
(check-expect (ordered-sub-list? (list #\a #\b #\c #\d) (list #\a #\b #\c))
              true)

(define (ordered-sub-list? lst1 lst2)
  (cond [(empty? lst2) true]
        [(empty? lst1) false]
        [(equal? (first lst1) (first lst2))
         (ordered-sub-list? (rest lst1) (rest lst2))]
        [else (ordered-sub-list? (rest lst1) lst2)]))


;; (enc-possible? string1 string2) produces true if the string2 may be hidden 
;;    inside the string1 using a Decryptor, and false otherwise
;; enc-possible?: Str Str -> Bool
;; requires: string2 to have length 3 and not containg *
;; Example:
(check-expect (enc-possible? "abcdefg" "bdg") true)
  
(define (enc-possible? string1 string2)
  (ordered-sub-list? (string->list string1) (string->list string2)))

;; Tests
(check-expect (enc-possible? "abcdefg" "beg") true)
(check-expect (enc-possible? "abcdefg" "efg") true)
(check-expect (enc-possible? "abcdefg" "abz") false)
(check-expect (enc-possible? "abcdefg" "bac") false)
