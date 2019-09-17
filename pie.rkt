;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 04, Question 4
;; *****************************************
;;

;;; useful symbols: 'small 'medium 'large 'custard

;; ==== Question 4a ============================================================

(define-struct pie (size fillings))
;; A Pie is a (make-pie (anyof 'small 'medium 'large) (listof Sym))
;; requires: fillings to be a non-empty list without repetition of elements
;;           fillings not equal to (cons 'custard empty)

;; Example pie
(define small-apple-pie (make-pie 'small (cons 'apple empty)))


;; ==== Question 4b ============================================================

;; (valid-symbol-list? lst) determines whether lst contains only symbols
;; valid-symbol-list?: (listof Sym) -> Bool
;; requires: lst to be non-empty
;; Example:
(check-expect (valid-symbol-list? (cons 'peanuts empty)) true)

(define (valid-symbol-list? lst)
  (cond [(empty? lst) true]
        [(symbol? (first lst)) (valid-symbol-list? (rest lst))]
        [else false]))

;; (not-custard? los) determines whether list equals (cons 'custard empty)
;; not-custard?: (listof Sym) -> Bool
;; requires: los to be non-empty
;; Example:
(check-expect (not-custard? (cons 'custard empty)) false)

(define (not-custard? los)
  (not (and (empty? (rest los)) (symbol=? 'custard (first los)))))

;; (symbol-occurrence? symbol los) determines whether symbol appears in los
;; symbol-occurrence?: Sym (listof Sym) -> Bool
;; requires: los to be non-empty
;; Example:
(check-expect (symbol-occurrence? 'apple (cons 'apple empty)) true)

(define (symbol-occurrence? symbol los)
  (cond [(empty? los) false]
        [(symbol=? symbol (first los)) true]
        [else (symbol-occurrence? symbol (rest los))]))

;; (valid-fillings? los) determines whether los is a valid filling list
;; for a Pie
;; valid-fillings?: (listof Sym) -> Bool
;; requires: los to be non-empty
;; Example:
(check-expect (valid-fillings? (cons 'apple (cons 'apple empty))) false)

(define (valid-fillings? los)
  (cond [(empty? (rest los)) true]
        [(symbol-occurrence? (first los) (rest los)) false]
        [else (valid-fillings? (rest los))]))

;; (valid-pie? x) determines whether x is a valid Pie
;; symbol-occurrence?: Any -> Bool
;; Example:
(check-expect (valid-pie? 3) false)

(define (valid-pie? x)
  (and (pie? x)
       (symbol? (pie-size x))
       (or (symbol=? (pie-size x) 'small)
           (symbol=? (pie-size x) 'medium)
           (symbol=? (pie-size x) 'large))
       (cons? (pie-fillings x))
       (valid-symbol-list? (pie-fillings x))
       (not-custard? (pie-fillings x))
       (valid-fillings? (pie-fillings x))))

;; Tests
(check-expect (valid-pie? (make-pie "small" empty)) false)
(check-expect (valid-pie? (make-pie 'xl empty)) false)
(check-expect (valid-pie? (make-pie 'small empty)) false)
(check-expect (valid-pie? (make-pie 'small (cons 12 empty))) false)
(check-expect (valid-pie? (make-pie 'large (cons 'custard empty))) false)
(check-expect (valid-pie? (make-pie 'medium
                                    (cons 'apple (cons 'apple empty)))) false)
(check-expect (valid-pie? (make-pie 'large
                                    (cons 'peach (cons 'apple empty)))) true)


;; ==== Question 4c ============================================================

;; (filling-swap fillings old-filling new-filling) produces a list changing the
;;   ocurrences of old-filling in fillings to new-filling 
;; filling-swap: (listof Sym) Sym Sym -> (list of Sym)
;; Example:
(check-expect (filling-swap (cons 'apple (cons 'peach empty))
                            'peach
                            'peanuts)
              (cons 'apple (cons 'peanuts empty)))

(define (filling-swap fillings old-filling new-filling)
  (cond [(empty? fillings) empty]
        [(symbol=? old-filling (first fillings))
         (cons new-filling
               (filling-swap (rest fillings) old-filling new-filling))]
        [else (cons (first fillings)
                    (filling-swap (rest fillings) old-filling new-filling))]))

;; (pie-swap pie old-filling new-filling) produces a Pie changing the ocurrences
;;   of old-filling in the fillings of pie to new-filling if valid 
;; pie-swap: Pie Sym Sym -> (Anyof Pie false)
;; Example:
(check-expect (pie-swap (make-pie 'medium (cons 'apple empty)) 'peanuts 'peach)
              (make-pie 'medium (cons 'apple empty)))

(define (pie-swap pie old-filling new-filling)
  (cond [(not (symbol-occurrence? old-filling (pie-fillings pie))) pie]
        [(and (symbol-occurrence? new-filling (pie-fillings pie))
              (cons? (rest (pie-fillings pie))))
         false]
        [(symbol-occurrence? new-filling (pie-fillings pie)) pie] 
        [(and (symbol=? 'custard new-filling)
              (empty? (rest (pie-fillings pie))))
         false]
        [else (make-pie (pie-size pie)
                        (filling-swap (pie-fillings pie)
                                      old-filling
                                      new-filling))]))

;; Tests
(check-expect (pie-swap (make-pie 'small (cons 'apple (cons 'peach empty)))
                        'apple
                        'peach)
              false)
(check-expect (pie-swap (make-pie 'small (cons 'apple empty)) 'apple 'apple)
              (make-pie 'small (cons 'apple empty)))
(check-expect (pie-swap (make-pie 'small (cons 'apple empty)) 'apple 'custard)
              false)
(check-expect (pie-swap (make-pie 'large (cons 'apple empty)) 'apple 'peach)
              (make-pie 'large (cons 'peach empty)))
(check-expect (pie-swap(make-pie 'large (cons 'apple (cons 'nuts empty)))
                       'apple
                       'peach)
              (make-pie 'large (cons 'peach (cons 'nuts empty))))


