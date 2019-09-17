;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pizza-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 07, Q2
;; *****************************************
;;

;; === Data definitions ========================================================

;; An Inventory List (InvList) is a  (listof (list Str Nat))
;; requires: each Str is unique

(define-struct invnode (item count left right))

;; An InventoryTree (InvTree) is one of:
;; * empty
;; * (make-invnode Str Nat InvTree InvTree)
;;   requires: item is string>? all items in left InvTree
;;             item is string<? all items in right InvTree

;; === Constants used in examples===============================================


(define example-ilist '(("mushroom" 50)
                        ("pepperoni" 25)
                        ("tomato sauce" 13)
                        ("broccoli" 0)))

(define example-ilist-with-pin '(("mushroom" 50)
                                 ("pepperoni" 25)
                                 ("tomato sauce" 13)
                                 ("broccoli" 0)
                                 ("pineapple" 2)))

(define example-itree
  (make-invnode "pepperoni" 25
                (make-invnode "broccoli" 0 empty
                              (make-invnode "mushroom" 50 empty empty))
                (make-invnode "tomato sauce" 13 empty empty)))

(define example-itree-2.0
  (make-invnode "pepperoni" 25
                (make-invnode "broccoli" 12 empty
                              (make-invnode "mushroom" 50 empty empty))
                (make-invnode "tomato sauce" 13 empty empty)))

(define example-itree-with-pin
  (make-invnode "pepperoni" 25
                (make-invnode "broccoli" 0 empty
                              (make-invnode "mushroom" 50 empty empty))
                (make-invnode "tomato sauce" 13
                              (make-invnode "pineapple" 2 empty empty) empty)))

;;             pepperoni
;;             /        \
;;      broccoli        tomato sauce
;;             \
;;            mushroom

;; ==== Question 2a ============================================================

;; (invtree-count a-invtree string) produces the number of available
;;   string in a-invtree
;; invtree-count: InvTree Str -> Nat
;; Example:
(check-expect (invtree-count empty "pineapple") 0)

(define (invtree-count a-invtree string)
  (cond [(empty? a-invtree) 0]
        [(string=? string (invnode-item a-invtree)) (invnode-count a-invtree)]
        [(string<? string (invnode-item a-invtree))
         (invtree-count (invnode-left a-invtree) string)]
        [else (invtree-count (invnode-right a-invtree) string)]))

;; Tests
(check-expect (invtree-count example-itree "pepperoni") 25)
(check-expect (invtree-count example-itree "broccoli") 0)
(check-expect (invtree-count example-itree-with-pin "pineapple") 2)

;; ==== Question 2b ============================================================

;; (invlist-count a-invlist string) produces the number of available
;;   string in a-invlist
;; invlist-count: InvList Str -> Nat
;; Example:
(check-expect (invlist-count empty "pineapple") 0)

(define (invlist-count a-invlist string)
  (cond [(empty? a-invlist) 0]
        [(string=? string (first (first a-invlist))) (second (first a-invlist))]
        [else (invlist-count (rest a-invlist) string)]))

;; Tests
(check-expect (invlist-count example-ilist "pepperoni") 25)
(check-expect (invlist-count example-ilist "broccoli") 0)
(check-expect (invlist-count example-ilist-with-pin "pineapple") 2)


;; ==== Question 2c ============================================================

;; (invtree-add a-invtree string n) produces an InvTree where the count 
;;     of string has been increased by n
;; invtree-add: InvTree Str Nat -> InvTree
;; Example:
(check-expect (invtree-add empty "meat" 5)
              (make-invnode "meat" 5 empty empty))

(define (invtree-add a-invtree string n)
  (cond [(empty? a-invtree) (make-invnode string n empty empty)]
        [(string=? string (invnode-item a-invtree))
         (make-invnode string
                       (+ n (invnode-count a-invtree))
                       (invnode-left a-invtree)
                       (invnode-right a-invtree))]
        [(string<? string (invnode-item a-invtree))
         (make-invnode (invnode-item a-invtree)
                       (invnode-count a-invtree)
                       (invtree-add (invnode-left a-invtree) string n)
                       (invnode-right a-invtree))]
        [else (make-invnode (invnode-item a-invtree)
                            (invnode-count a-invtree)
                            (invnode-left a-invtree)
                            (invtree-add (invnode-right a-invtree) string n))]))

;; Tests
(check-expect (invtree-add example-itree "pineapple" 2) example-itree-with-pin)
(check-expect (invtree-add example-itree "broccoli" 12) example-itree-2.0)

;; ==== Question 2d ============================================================

;; (invtree-low? a-invtree n) determines whether a-invtree contains at least one
;;   item with a count less than or equal to n
;; inv-tree-low?: InvTree Nat -> Bool
;; Example:
(check-expect (invtree-low? empty 10) false)

(define (invtree-low? a-invtree n)
  (cond [(empty? a-invtree) false]
        [(<= (invnode-count a-invtree) n) true]
        [else (local [(define left (invtree-low? (invnode-left a-invtree) n))
                      (define right (invtree-low? (invnode-right a-invtree) n))]
                
                (or left right))]))

;; Tests
(check-expect (invtree-low? (make-invnode "hi" 20 empty empty) 30) true)
(check-expect (invtree-low? example-itree 30) true)
(check-expect (invtree-low? example-itree 0) true)
(check-expect (invtree-low? example-itree-2.0 9) false)

;; ==== Question 2e ============================================================

;; (invtree->invlist a-invtree) transform a-invtree into an InvList
;; inv-tree->invlist: InvTree -> InvList
;; Example:
(check-expect (invtree->invlist empty) empty)

(define (invtree->invlist a-invtree)
  (cond [(empty? a-invtree) empty]
        [else (append (invtree->invlist (invnode-left a-invtree))
                      (list (list (invnode-item a-invtree)
                                  (invnode-count a-invtree)))
                      (invtree->invlist (invnode-right a-invtree)))]))

;; Tests
(check-expect (invtree->invlist example-itree)
              '(("broccoli" 0) ("mushroom" 50) ("pepperoni" 25)
                               ("tomato sauce" 13)))
(check-expect (invtree->invlist example-itree-with-pin)
              '(("broccoli" 0) ("mushroom" 50) ("pepperoni" 25)
                               ("pineapple" 2) ("tomato sauce" 13)))
