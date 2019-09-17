;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname weather) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 02, Question 2
;; *****************************************
;;


;; Presure symbols
;; 

;; Pressure change symbols
;; 'steady 'rising-slowly 'rising-quickly 'falling-slowly 'falling-quickly

;; ==== Question 2a ========================

;; Rain-p is (anyof 'low 'moderate 'high)
;; Rain-pc is (anyof 'steady 'rising-slowly 'rising-quickly
;;                   'falling-slowly 'falling-quickly)

;; (going-to-rain?/cond rain pressure pressure-change) produces a prediction
;;  of the future raining-state
;; going-to-rain?/cond rain pressure: Bool Rain-p Rain-pc -> Bool
;; Example:
(check-expect (going-to-rain?/cond true 'low 'steady) true)

(define (going-to-rain?/cond rain pressure pressure-change)
  (cond
    [(symbol=? pressure-change 'steady) rain]
    [(symbol=? pressure-change 'rising-quickly) false]
    [(symbol=? pressure-change 'rising-slowly)
     (cond [(symbol=? pressure 'low) rain]
           [else false])]
    [(symbol=? pressure-change 'falling-quickly) true]
    [(symbol=? pressure-change 'falling-slowly)
     (cond [(symbol=? pressure 'low) true]
           [(symbol=? pressure 'moderate) rain]
           [else false])]))

;;Tests
(check-expect (going-to-rain?/cond true 'moderate 'rising-quickly) false)
(check-expect (going-to-rain?/cond true 'low 'rising-slowly) true)
(check-expect (going-to-rain?/cond false 'high 'rising-slowly) false)
(check-expect (going-to-rain?/cond true 'low 'rising-slowly) true)
(check-expect (going-to-rain?/cond false 'low 'rising-slowly) false)
(check-expect (going-to-rain?/cond false 'high 'falling-quickly) true)
(check-expect (going-to-rain?/cond false 'low 'falling-slowly) true)
(check-expect (going-to-rain?/cond false 'moderate 'falling-slowly) false)
(check-expect (going-to-rain?/cond true 'moderate 'falling-slowly) true)
(check-expect (going-to-rain?/cond true 'high 'falling-slowly) false)


;; ==== Question 2b ========================

;; (going-to-rain?/cond rain pressure pressure-change) produces a prediction
;;  of the future raining-state
;; going-to-rain?/cond rain pressure: Bool Sym Sym -> Bool
;; Example:
(check-expect (going-to-rain?/bool true 'low 'steady) true)

(define (going-to-rain?/bool rain pressure pressure-change)
  (or
   (and (symbol=? pressure-change 'steady) rain)
   (and (symbol=? pressure-change 'rising-slowly)
        (symbol=? pressure 'low)
        rain)
   (symbol=? pressure-change 'falling-quickly)
   (and (symbol=? pressure-change 'falling-slowly)
        (or (symbol=? pressure 'low)
            (and (symbol=? pressure 'moderate) rain)))))

;;Tests
(check-expect (going-to-rain?/bool true 'moderate 'rising-quickly) false)
(check-expect (going-to-rain?/bool true 'low 'rising-slowly) true)
(check-expect (going-to-rain?/bool false 'high 'rising-slowly) false)
(check-expect (going-to-rain?/bool true 'low 'rising-slowly) true)
(check-expect (going-to-rain?/bool false 'low 'rising-slowly) false)
(check-expect (going-to-rain?/bool false 'high 'falling-quickly) true)
(check-expect (going-to-rain?/bool false 'low 'falling-slowly) true)
(check-expect (going-to-rain?/bool false 'moderate 'falling-slowly) false)
(check-expect (going-to-rain?/bool true 'moderate 'falling-slowly) true)
(check-expect (going-to-rain?/bool true 'high 'falling-slowly) false)
