;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 06, Q1
;; *****************************************
;;


;; ==== Question 1a ============================================================

;; (fibonacci-acc m n fm-1 fm-2) produces the n-th fibonacci number given
;;   the two terms before the m-th term: fm-1 and fm-2
;; fibonacci-acc: Nat Nat Nat Nat -> Nat
;; requires: n>=m>=2
;;           fm-1 and fm-2 are the (m-1)-th and (m-2)-th terms of the
;;           fibonacci sequence  
;; Example:
(check-expect (fibonacci-acc 2 2 1 0) 1)

(define (fibonacci-acc m n fm-1 fm-2)
  (cond [(<= n 1) n]
        [(= m n) (+ fm-1 fm-2)]
        [else (fibonacci-acc (add1 m) n (+ fm-1 fm-2) fm-1)]))


;; (fibonacci-acc n) produces the n-th fibonacci number 
;; fibonacci: Nat -> Nat
;; Example:
(check-expect (fibonacci 0) 0)

(define (fibonacci n)
  (fibonacci-acc 2 n 1 0))

;; Tests
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 3) 2)
(check-expect (fibonacci 7) 13)


;; ==== Question 1b ============================================================

;; (my-list-ref lst pos) produces the element in lst whose position is pos
;; my-list-ref: (listof Any) Nat -> Any
;; requires: (length lst) > pos
;; Example:
(check-expect (my-list-ref '(a b c) 0) 'a)

(define (my-list-ref lst pos)
  (cond [(= 0 pos) (first lst)]
        [else (my-list-ref (rest lst) (sub1 pos))]))

;; Tests
(check-expect (my-list-ref '(a b c) 1) 'b)
(check-expect (my-list-ref '(a b c d e f) 5) 'f)


;; ==== Question 1c ============================================================

;; (list-of-char n char) produces a list that contains exactly n times the
;;   character char
;; list-of-char: Nat Char -> (listof Char)
;; Example:
(check-expect (list-of-char 2 #\z) '(#\z #\z))

(define (list-of-char n char)
  (cond [(= n 0) empty]
        [else (cons char (list-of-char (sub1 n) char))]))


;; (string-of-char n char) produces a string that contains exactly n times the
;;   character char
;; list-of-char: Nat Char -> Str
;; Example:
(check-expect (string-of-char 0 #\z) "")

(define (string-of-char n char)
  (list->string (list-of-char n char)))

;; Tests
(check-expect (string-of-char 5 #\z) "zzzzz")
(check-expect (string-of-char 7 #\a) "aaaaaaa")


;; ==== Question 1d ============================================================

;; (replace-vowels/list-acc lochars acc) replaces the first vowel in lochars
;;   with acc repetitions of #\x, the second vowel with (add1 acc) repetitions 
;;   of #\x, and so on
;; replace-vowels/list-acc: (listof Char) Nat -> (listof Char)
;; Example:
(check-expect (replace-vowels/list-acc '(#\a #\z) 2) '(#\x #\x #\z))

(define (replace-vowels/list-acc lochars acc)
  (cond [(empty? lochars) empty]
        [(or (char=? (first lochars) #\a) (char=? (first lochars) #\e)
             (char=? (first lochars) #\i) (char=? (first lochars) #\o)
             (char=? (first lochars) #\u))
         (append (list-of-char acc #\x)
                 (replace-vowels/list-acc (rest lochars) (add1 acc)))]
        [else (cons (first lochars)
                    (replace-vowels/list-acc (rest lochars) acc))]))


;; (replace-vowels string) replaces the i-th vowel of string with i repetitions
;;   of #\x
;; replace-vowels: Str -> Str
;; Example:
(check-expect (replace-vowels "") "")

(define (replace-vowels string)
  (list->string (replace-vowels/list-acc (string->list string) 1)))

;; Tests
(check-expect (replace-vowels "a") "x")
(check-expect (replace-vowels "the") "thx")
(check-expect (replace-vowels "hii") "hxxx")
(check-expect (replace-vowels "foo") "fxxx")
(check-expect (replace-vowels "hulu") "hxlxx")
(check-expect (replace-vowels "CS") "CS")
(check-expect (replace-vowels "hello world") "hxllxx wxxxrld")
