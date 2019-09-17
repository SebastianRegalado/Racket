;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname score) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 02, Question 1
;; *****************************************
;;

;; (tetris-score c-level lines) produces the number of scored points  
;;   in tetris given c-level and lines
;; tetris-score: Nat Nat -> Nat
;; Example:
(check-expect (tetris-score 19 6) 0)

(define (tetris-score current-level lines)
  (cond
    [(>= lines 5) 0]
    [else (* (+ 1 current-level)
             (cond [(= lines 1) 40]
                   [(= lines 2) 100]
                   [(= lines 3) 300]
                   [else 1200]))]))

;;Tests
(check-expect (tetris-score 12 1) 520)
(check-expect (tetris-score 7 2) 800)
(check-expect (tetris-score 5 3) 1800)
(check-expect (tetris-score 9 4) 12000)
