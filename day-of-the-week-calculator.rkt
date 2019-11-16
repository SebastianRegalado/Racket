;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname day-of-the-week-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;; Symbols for days of the week
;; 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday


;; In this solution we use Zeller's Rule in order to find the day of
;; the week of a given date. For more information check
;; http://mathforum.org/dr.math/faq/faq.calendar.html
;; We define years-number, months-number, days-number, C, D, and f as in 
;; the above algorithm. fr represents the (positive) remainder of mod 7 

;; (years-number full-date) produces the number of years in full-date
(define (years-number full-date)
  (quotient full-date 10000))

;; (months-number full-date) produces the corresponding month-number of full-date
(define (months-number full-date)
  (+ (remainder (+ 9 (remainder (quotient full-date 100) 100)) 12) 1))

;; (C full-date)
;; C: Nat -> Nat

(define (C full-date)
  (quotient (years-number full-date) 100))


;; (days-number full-date)
;; days-number: Nat -> Nat

(define (days-number full-date)
  (remainder full-date 100))


;; (D full-date)
;; D: Nat -> Nat

(define (D full-date)
  (- (remainder (years-number full-date) 100)
     (cond [(or (= (months-number full-date) 11)
                (= (months-number full-date) 12)) 1]
           [else 0])))

;; (f full-date)
;; f: Nat -> Nat

(define (f full-date)
  (+ (days-number full-date)
     (quotient (- (* 13 (months-number full-date)) 1) 5)
     (D full-date)
     (quotient (D full-date) 4)
     (quotient (C full-date) 4)
     (* -2 (C full-date))))

;; (fr full-date)
;; f: Nat -> Nat

(define (fr full-date)
  (+ (remainder (f full-date) 7)
     (cond [(>= (f full-date) 0) 0]
           [else 7])))


;; (day-of-week n) produces days of the week 
;;  for particular remainders n (mod 7) 
;; day-of-week: Nat -> Sym
;; Example:
(check-expect (day-of-week 3) 'Wednesday)

(define (day-of-week n)
  (cond [(= n 1) 'Monday]
        [(= n 2) 'Tuesday]
        [(= n 3) 'Wednesday]
        [(= n 4) 'Thrusday]
        [(= n 5) 'Friday]
        [(= n 6) 'Saturday]
        [else 'Sunday]))

;; (date->day-of-week full-date) produces the day of the  
;;  week of full-date  
;; date->day-of-week: Nat -> Sym
;; Example:
(check-expect (date->day-of-week 17560205) 'Thrusday)

(define (date->day-of-week full-date)
  (day-of-week (fr full-date)))

;;Tests
(check-expect (date->day-of-week 18650102) 'Monday)
(check-expect (date->day-of-week 18000717) 'Thrusday)
(check-expect (date->day-of-week 19001125) 'Sunday)
(check-expect (date->day-of-week 19990807) 'Saturday)
(check-expect (date->day-of-week 17821209) 'Monday)
(check-expect (date->day-of-week 18620204) 'Tuesday)
(check-expect (date->day-of-week 18950326) 'Tuesday)
(check-expect (date->day-of-week 19240430) 'Wednesday)
(check-expect (date->day-of-week 17840627) 'Sunday)
(check-expect (date->day-of-week 18920914) 'Wednesday)
(check-expect (date->day-of-week 18121016) 'Friday)
(check-expect (date->day-of-week 17591019) 'Friday)
(check-expect (date->day-of-week 17590419) 'Thrusday)
(check-expect (date->day-of-week 17590619) 'Tuesday)
(check-expect (date->day-of-week 17590919) 'Wednesday)
(check-expect (date->day-of-week 17600119) 'Saturday)
(check-expect (date->day-of-week 17600319) 'Wednesday)
(check-expect (date->day-of-week 17600719) 'Saturday)
(check-expect (date->day-of-week 17600819) 'Tuesday)
(check-expect (date->day-of-week 17601119) 'Wednesday)
(check-expect (date->day-of-week 17601219) 'Friday)
