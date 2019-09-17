;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment A9, Q3
;; *****************************************
;;



;; === Constants used in examples===============================================


(define test-a (list 2 4 6 8 10 12))
(define test-b (list 1 11 33 67 113))
(define test-c (list 2 4 8 16 32 64))
(define test-d (list 1 1 2 3 5 8 13))
(define test-e (list 0 1 1 2 3 5 8))
(define test-f (list 1 1 3 5 11 21 43))
(define test-g (list 2 0 0 0 0 0))


;; ==== Question 3a=============================================================

;; (solution? f lon) checks that f generates the values in the list of integers
;;   lon, where f takes a position in the list as an argument, starting at 0.
;; solution?: (Nat -> Int) (listof Int) -> Bool
;; Example:
(check-expect (solution? (lambda (i) (* 2 (+ i 1))) (list 2 4 6 8 10 12)) true)

(define (solution? f lon)
  (equal? (build-list (length lon) f) lon))

;; Tests
(check-expect (solution? (lambda (i) i) (list 0 1 2 3 4 5)) true)
(check-expect (solution? (lambda (i) i) (list 1 2 3 4 5 6)) false)
(check-expect (solution? (lambda (i) i) empty) true)


;; ==== Question 3b=============================================================

;; (guess-quadratic lon) guesses that the number sequence lon has a
;;   quadratic solution and produces the guess.
;; guess-quadratic: (listof Num) -> (Nat -> Num)
;; Example:
(check-expect (solution? (guess-quadratic test-a) test-a) true)

(define (guess-quadratic lon)
  (cond [(empty? lon) (lambda (x) 0)]
        [(empty? (rest lon)) (lambda (x) (first lon))]
        [(empty? (rest (rest lon)))
         (lambda (x) (+ (* x (- (second lon) (first lon))) (first lon)))]
        [else (lambda (x)
                (+ (* x x 0.5 (- (+ (first lon) (third lon))
                                 (* 2 (second lon))))
                   (* x 0.5 (- (- (* 4 (second lon)) (third lon))
                               (* 3 (first lon))))
                   (first lon)))]))

;; Tests
(check-expect (solution? (guess-quadratic empty) '(0 0)) true)
(check-expect (solution? (guess-quadratic '(1)) '(1 1 1)) true)
(check-expect (solution? (guess-quadratic '(0 1)) '(0 1 2 3 4)) true)
(check-expect (solution? (guess-quadratic test-b) test-b) true)
(check-expect (solution? (guess-quadratic test-c) test-c) false)


;; ==== Question 3c=============================================================

;; (try-quadratic lon) tries solving the number sequence lon with a quadratic.
;;   It produces the solution if it works and empty if it doesn't.
;; try-quadratic: (listof Num) -> (anyof (Nat -> Num) empty)
;; Example:
(check-expect ((try-quadratic test-a) 6) 14)

(define (try-quadratic lon)
  (cond [(solution? (guess-quadratic lon) lon) (guess-quadratic lon)]
        [else empty]))

;; Tests
(check-expect ((try-quadratic test-a) 1000000) 2000002)
(check-expect ((try-quadratic test-b) 5) 171)
(check-expect (try-quadratic test-c) empty)

;; ==== Question 3d=============================================================

;; (recursive n a b n1 n0) produces the n-th element of a recursive sequence
;;   whose first element is n0, second element is n1, and the i-th element is
;;   a*((i-1)-th element) + b*((i-2)-th element) for i>1.
;; recursive: Nat Num Num Num Num -> Num
;; Example:
(check-expect (recursive 2 1 1 1 1) 2)

(define (recursive n a b n1 n0)
  (cond [(= n 0) n0]
        [(= n 1) n1]
        [else
         (local
           [(define (recursive-acc m fm-1 fm-2)
              (cond [(= m n) (+ (* a fm-1) (* b fm-2))]
                    [else
                     (recursive-acc (add1 m)
                                    (+ (* a fm-1) (* b fm-2)) fm-1)]))]
           (recursive-acc 2 n1 n0))]))


;; (guess-recursive lon) guesses that the number sequence lon has a
;;   recursive solution and produces the guess.
;; guess-recursive: (listof Num) -> (Nat -> Num)
;; Example:
(check-expect ((guess-recursive '(1)) 2) 1)

(define (guess-recursive lon)
  (cond [(empty? lon) (lambda (x) 0)]
        [(empty? (rest lon)) (lambda (x) (first lon))]
        [(= 0 (first lon) (second lon)) (lambda (x) 0)]
        [(empty? (rest (rest lon)))
         (lambda (x) (recursive x 1 1 (second lon) (first lon)))]
        [(= 0 (- (* (first lon) (third lon)) (sqr (second lon))))
         (lambda (x) (* (first lon) (expt (/ (second lon) (first lon)) x)))]
        [(and (empty? (rest (rest (rest lon)))) (= 0 (first lon)))
         (lambda (x) (recursive x (/ (third lon) (second lon)) 0
                                (second lon) (first lon)))]
        [(empty? (rest (rest (rest lon))))
         (lambda (x) (recursive x 0 (/ (third lon) (first lon))
                                (second lon) (first lon)))]
        [else (local [(define a (/ (- (* (fourth lon) (first lon))
                                      (* (second lon) (third lon)))
                                   (- (* (first lon) (third lon))
                                      (sqr (second lon)))))
                      (define b (/ (- (sqr (third lon))
                                      (* (second lon) (fourth lon)))
                                   (- (* (first lon) (third lon))
                                      (sqr (second lon)))))]
                (lambda (x) (recursive x a b (second lon) (first lon))))]))

;; Tests
(check-expect ((guess-recursive '()) 2) 0)
(check-expect ((guess-recursive '(0 0 1)) 2) 0)
(check-expect ((guess-recursive '(1 1)) 3) 3)
(check-expect ((guess-recursive '(0 4 8)) 4) 32)
(check-expect ((guess-recursive '(2 0 4)) 4) 8)


;; (try-recursive lon) tries solving the number sequence lon with a recursive.
;;   It produces the solution if it works and empty if it doesn't.
;; try-quadratic: (listof Num) -> (anyof (Nat -> Num) empty)
;; Example:
(check-expect ((try-recursive test-d) 7) 21)

(define (try-recursive lon)
  (cond [(solution? (guess-recursive lon) lon) (guess-recursive lon)]
        [else empty]))

;; Tests
(check-expect ((try-recursive test-d) 100) 573147844013817084101)
(check-expect ((try-recursive test-e) 7) 13)
(check-expect ((try-recursive test-f) 7) 85)
(check-expect ((try-recursive test-g) 0) 2)
(check-expect ((try-recursive test-g) 100) 0)
(check-expect (try-recursive test-b) empty)


;; ==== Question 3e=============================================================

;; (solve lon)  tries both the quadratic and recursive patterns, in that order,
;;   and returns a solution, if one exists. Otherwise, return the empty list.
;; solve: (listof Num) -> (anyof (Nat -> Num) empty)
;; Example:
(check-expect ((solve test-a) 6) 14)

(define (solve lon)
  (cond [(not (empty? (try-quadratic lon))) (guess-quadratic lon)]
        [(not (empty? (try-recursive lon))) (guess-recursive lon)]
        [else empty]))

(check-expect ((solve test-a) 1000000) 2000002)
(check-expect ((solve test-b) 5) 171)
(check-expect ((solve test-c) 8) 512)
(check-expect ((solve test-d) 7) 21)
(check-expect ((solve test-d) 100) 573147844013817084101)
(check-expect ((solve test-e) 7) 13)
(check-expect ((solve test-f) 7) 85)
(check-expect ((solve test-g) 0) 2)
(check-expect ((solve test-g) 100) 0)
(check-expect (solve '(1 1 2 1 2 3 1 2 3 4)) empty)
