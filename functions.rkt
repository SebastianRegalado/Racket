;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp")) #t)))
;;
;; ***************************************************
;; Sebastian Regalado (20759473)
;; CS 135 Winter 2019
;; Assignment 01, Problem 2
;; ***************************************************
;;


;;(a)
;; (volume r) produces the volume of a sphere
;;   of radius r
;; volume: Num -> Num
;; Example:
(check-within (volume 10) 4188.790 0.01)

(define (volume r)
  (* (/ 4 3) pi (expt r 3)))


;;(b)
;;Useful constant
(define G 6.674e-11)

;; (escape M r) produces the escape speed of a rocket 
;;   given the mass and radius of the associated body  
;; escape: Num Num -> Num
;; Example:
(check-expect (escape 1e11 13.348) 1)

(define (escape M r)
  (sqrt (/ (* 2 G M) r)))


;;(c)
;; (fib n) produces and aproximation to the nth 
;;   fibonacci number  
;; fib: Nat -> Num
;; Example:
(check-within (fib 1) 1 0.001)

(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (- 1 phi))
(define (fib n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5)))


;;(d)
;; (partition-size-approximation n) produces and aproximation to the number
;;   of partitions for the positive integer n  
;; partition-size-approximation: Nat -> Num
;; Example:
(check-within (partition-size-approximation 4) 6.1000 0.01)

(define (partition-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3)))
     (exp (* pi (sqrt (/ (* 2 n) 3))))))

















