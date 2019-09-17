;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp")) #f)))
;;Question 2
;;(a)
(define (volume-a-sphere r)
  (* (/ 4 3) pi (expt r 3)))

;;(b)
(define G 6.674e-11)
(define (escape-speed M r)
  (sqrt (/ (* 2 G M) r)))

;;(c)
(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (- 1 phi))
(define (fibonacci n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5)))

;;(d)
(define (partion-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3)))
     (exp (* pi (sqrt (/ (* 2 n) 3))))))

;;Question 3
;;(a)
(define miles-to-100km (/ 100 1.609344))
(define gallons-to-liters 3.785411784)
(define (mpg->lp100km a-mpg-fe)
  (/ gallons-to-liters (* a-mpg-fe miles-to-100km)))

;;(b)
(define yards-to-rods (/ 1 5.5))
(define miles-to-yards 1760)
(define gallons-to-hogsheads (/ 1 79))
(define (mpg->rph a-mpg-fe)
  (* a-mpg-fe miles-to-yards yards-to-rods (/ 1 gallons-to-hogsheads)))

;;Question 4
;;(a)
(define midterm-weight 0.25)
(define final-weight 0.25)
(define assign-weight 0.25)
(define participation-marks 5)
(define (final-cs135-grade midterm-g final-g assign-g)
  (+ participation-marks
     (* midterm-weight midterm-g)
     (* final-weight final-g)
     (* assign-weight assign-g)))

;;(b)
(define minimum-grade-to-pass 60)
(define (cs135-final-exam-grade-needed midterm-g assign-g)
  (/ (- minimum-grade-to-pass (+ participation-marks
                                 (* midterm-weight midterm-g)
                                 (* assign-weight assign-g)))
     final-weight))

;;Question 5 Bonus
(define (cs135-participation total-noq no-incorrect-q no-correct-q)
  (/ (- (+ no-incorrect-q (* 2 no-correct-q))
        (+ (min (max 0 (- (* 1/4 total-noq) (- total-noq (+ no-incorrect-q no-correct-q)))) no-incorrect-q)
           (* 2 (min (max 0 (- (* 1/4 total-noq) (- total-noq no-correct-q))) no-correct-q))))
     (* 3/2 total-noq)))
