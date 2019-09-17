;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment A9, Q1
;; *****************************************
;;

;; === Data definitions ========================================================

;; An Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))


;; === Constants used in examples===============================================


(define a-graph
  '((A (C D E))
    (B (E J))
    (C ())
    (D (F J))
    (E (K))
    (F (K H))
    (H ())
    (J (H))
    (K ())))


;; ==== Question 1 Code=========================================================

;; (neighbours v G) produces a list of all the neighbours of v in G
;; neighbours: Node Graph -> (listof Node)
;; requires: v is in G
;; Example:
(check-expect (neighbours 'J a-graph) '(H))

(define (neighbours v G)
  (cond [(symbol=? v (first (first G))) (second (first G))]
        [else (neighbours v (rest G))]))


;; (nodes G) produces a list of all the nodes of G
;; nodes: Graph -> (listof Node)
;; Example:
(check-expect (nodes a-graph) '(A B C D E F H J K))

(define (nodes G)
  (cond [(empty? G) empty]
        [else (cons (first (first G)) (nodes (rest G)))]))


;; (valid-route? route G) determines whether route is a valid path in G
;; valid-route: (listof Node) Graph -> Bool
;; Example:
(check-expect (valid-route? '(A D J H) a-graph) true)

(define (valid-route? route G)
  (local [(define node-list (nodes G))]
    (cond [(empty? route) true]
          [(not (member? (first route) node-list)) false]
          [(empty? (rest route)) true]
          [(member? (second route) (neighbours (first route) G))
           (valid-route? (rest route) G)]
          [else false])))

;; Tests
(check-expect (valid-route? '(A B C) a-graph) false)
(check-expect (valid-route? '(A B C) empty) false)
(check-expect (valid-route? '(A J H) empty) false)
(check-expect (valid-route? empty a-graph) true)
(check-expect (valid-route? empty empty) true)
(check-expect (valid-route? '(X Y Z) a-graph) false)
