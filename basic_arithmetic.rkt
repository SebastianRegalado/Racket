;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp")) #f)))

;; Volume of a sphere of radius r
(define (volume-a-sphere r)
  (* (/ 4 3) pi (expt r 3)))

;; Equation for escape velocity
(define G 6.674e-11)
(define (escape-speed M r)
  (sqrt (/ (* 2 G M) r)))

;; Explicit formula for fibonacci terms
(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (- 1 phi))
(define (fibonacci n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5)))

;; Mathematicians Hardy and Ramanujan developed the following formula
;; to approximate the number of partitions for the positive integer n
(define (partition-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3)))
     (exp (* pi (sqrt (/ (* 2 n) 3))))))

;; Converts miles/gallon to liters/100km
(define miles-to-100km (/ 100 1.609344))
(define gallons-to-liters 3.785411784)
(define (mpg->lp100km a-mpg-fe)
  (/ gallons-to-liters (* a-mpg-fe miles-to-100km)))

;; Converts miles/gallon to rod/hogshead
(define yards-to-rods (/ 1 5.5))
(define miles-to-yards 1760)
(define gallons-to-hogsheads (/ 1 79))
(define (mpg->rph a-mpg-fe)
  (* a-mpg-fe miles-to-yards yards-to-rods (/ 1 gallons-to-hogsheads)))

;; Grade Calculator
(define midterm-weight 0.25)
(define final-weight 0.25)
(define assign-weight 0.25)
(define participation-marks 5)
(define (final-grade midterm-g final-g assign-g)
  (+ participation-marks
     (* midterm-weight midterm-g)
     (* final-weight final-g)
     (* assign-weight assign-g)))
     
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
