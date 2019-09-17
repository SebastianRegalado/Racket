;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 03, Question 3
;; *****************************************
;;


(define-struct clicker (correct incorrect absent))
;; A Clicker is a (make-clicker Nat Nat Nat)

(define-struct cs135mark (clickers assignments midterm final))
;; A CS135Mark is a (make-cs135mark Clicker Num Num Num)
;; requires: assignments, midterm, and final are exact and non-negative

;; ==== Question 3a ========================
(define correct-value 2)
(define incorrect-value 1)

;; (total-questions clicker) produces the total number of clicker
;;   questions in clicker
;; total-questions: Clicker -> Nat
;; Example:
(check-expect (total-questions (make-clicker 20 10 10)) 40)

(define (total-questions clicker)
  (+ (clicker-correct clicker)
     (clicker-incorrect clicker)
     (clicker-absent clicker)))

;; (total-counted-questions clicker) produces the total number of clicker
;;   questions in clicker that counts to calculate a participation mark
;; total-counted-questions: Clicker -> Nat
;; Example:
(check-expect (total-counted-questions (make-clicker 20 10 10)) 30)

(define (total-counted-questions clicker)
  (* 0.75 (total-questions clicker)))

;; (clicker-grade clicker) produces the participation mark of a student
;;   based on clicker
;; clicker-grade: Clicker -> Num
;; Example:
(check-expect (clicker-grade (make-clicker 20 10 10)) (/ 500 6))

(define (clicker-grade clicker)
  (cond [(>= (clicker-absent clicker) (* 0.25 (total-questions clicker)))
         (* (/ (+ (* correct-value (clicker-correct clicker))
                  (* incorrect-value (clicker-incorrect clicker)))
               (* correct-value (total-counted-questions clicker)))
            100)]
        [(< (clicker-correct clicker) (* 0.75 (total-questions clicker)))
         (* (/ (+ (* correct-value (clicker-correct clicker))
                  (* incorrect-value (- (* 0.75 (total-questions clicker))
                                        (clicker-correct clicker))))
               (* correct-value (total-counted-questions clicker)))
            100)]
        [else 100]))

;; Tests
(check-expect (clicker-grade (make-clicker 20 10 10)) (/ 500 6))
(check-expect (clicker-grade (make-clicker 15 5 20)) (/ 350 6))
(check-expect (clicker-grade (make-clicker 20 15 5)) (/ 500 6))
(check-expect (clicker-grade (make-clicker 30 5 5)) 100)


;; ==== Question 3b ========================
(define midterm-weight 0.25)
(define final-weight 0.5)
(define assign-weight 0.2)
(define participation-weight 0.05)

;; (cs135-grade cs135mark) produces the earned cs135 grade of a student
;;   given cs135mark
;; cs135-grade: CS135Mark -> Num
;; Example:
(check-expect (cs135-grade (make-cs135mark (make-clicker 30 0 10) 100 100 100))
              100)

(define (cs135-grade cs135mark)
  (+ (* participation-weight (clicker-grade (cs135mark-clickers cs135mark)))
     (* assign-weight (cs135mark-assignments cs135mark))
     (* midterm-weight (cs135mark-midterm cs135mark))
     (* final-weight (cs135mark-final cs135mark))))

;; Tests
(check-expect (cs135-grade (make-cs135mark (make-clicker 45 0 15) 100 65 80))
              81.25)
(check-expect (cs135-grade (make-cs135mark (make-clicker 45 0 15) 100 70 80))
              82.5)
(check-expect (cs135-grade (make-cs135mark (make-clicker 30 5 5) 100 100 100))
              100)
(check-expect (cs135-grade (make-cs135mark (make-clicker 30 5 5) 120 100 100))
              104)

;; ==== Question 3c ========================

;; (registrar-grade cs135mark) produces the final cs135 grade of a student
;;   given cs135mark
;; registrar-grade: CS135Mark -> Num
;; Example:
(check-expect (registrar-grade
               (make-cs135mark (make-clicker 30 0 10) 100 100 100)) 100)

(define (registrar-grade cs135mark)
  (cond [(>= (cs135-grade cs135mark) 100) 100]
        [(< (- (cs135-grade cs135mark) (floor (cs135-grade cs135mark))) 0.5)
         (floor (cs135-grade cs135mark))]
        [else (ceiling (cs135-grade cs135mark))]))

;; Tests
(check-expect (registrar-grade
               (make-cs135mark (make-clicker 45 0 15) 100 65 80)) 81)
(check-expect (registrar-grade
               (make-cs135mark (make-clicker 45 0 15) 100 70 80)) 83)
(check-expect (registrar-grade
               (make-cs135mark (make-clicker 20 10 10) 90 90 150)) 100)











