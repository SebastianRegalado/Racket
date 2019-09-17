;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 08, Question 4
;; *****************************************
;;

;; (partition pred lst) produces a two element list where the first list
;;   contains the elements of lst that satisfy pred and the second list
;;   contains the elements of lst that do not satisfy pred
;; partition: (Any -> Bool) (listof Any) -> (list (listof Any) (listof Any))
;; Example:
(check-expect (partition boolean? empty) '(() ()))

(define (partition pred lst)
  (local [;; (partition/acc lst sofar) adds the elements in lst that satisfy
          ;;   pred to the first element of sofar and adds the elements in lst 
          ;;   that do not satisfy pred to the second element of sofar
          ;; partition: (listof Any) (list (listof Any) (listof Any))
          ;;             -> (list (listof Any) (listof Any))
          (define (partition/acc lst sofar)
            (cond [(empty? lst) sofar]
                  [(pred (first lst))
                   (partition/acc (rest lst)
                                  (list (cons (first lst) (first sofar))
                                        (second sofar)))]
                  [else (partition/acc (rest lst)
                                  (list (first sofar)
                                        (cons (first lst) (second sofar))))]))]
    
    (partition/acc (reverse lst) '(() ()))))

;; Tests
(check-expect (partition odd? '(1 2 3 4 5)) '((1 3 5) (2 4)))
(check-expect (partition even? '(1 2 3 4 5)) '((2 4) (1 3 5)))
(check-expect (partition symbol? '(1 2 a b c)) '((a b c) (1 2)))


