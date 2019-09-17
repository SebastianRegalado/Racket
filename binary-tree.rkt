;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binary-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 06, Q3
;; *****************************************
;;

;; === Data definitions ========================================================

;; A Binary Search Tree (BST) is one of:
;; * empty
;; * Node

(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;;           key < every key in right BST


;; Example Trees
(define my-bst
  (make-node 5
             (make-node 3 empty empty)
             (make-node 9
                        (make-node 7 empty empty)
                        empty)))
;;     5      |
;;    / \     |
;;   3   9    | height is 3, longest path is 5 -> 9 -> 7
;;      /     |
;;     7      |
                          

(define another-bst
  (make-node 5
             (make-node 3
                        (make-node 1
                                   (make-node 0 empty empty)
                                   (make-node 2 empty empty))
                        (make-node 4 empty empty))
             (make-node 9 empty empty)))

;;      5     |
;;     / \    |
;;    3   9   |
;;   / \      | height is 4, longest paths are 5->3->1->0 and 5->3->1->2
;;  1   4     |
;; / \        |
;;0   2       |

;; ==== Question 3a ============================================================

;; (bst-count a-bst) produces the number of nodes in a-bst
;; bst-count: BST -> Nat
;; Example:
(check-expect (bst-count empty) 0)

(define (bst-count a-bst)
  (cond [(empty? a-bst) 0]
        [else (+ 1 (bst-count (node-left a-bst))
                 (bst-count (node-right a-bst)))]))

;; Tests
(check-expect (bst-count my-bst) 4)
(check-expect (bst-count another-bst) 7)

;; ==== Question 3b ============================================================

;; (bst-add num a-bst) adds a node to a-bst whose key is num if there is no node
;;   in a-bst with num as its key, otherwise produces a-bst
;; bst-add: Nat BST -> BST
;; Example:
(check-expect (bst-add 3 empty) (make-node 3 empty empty))

(define (bst-add num a-bst)
  (cond [(empty? a-bst) (make-node num empty empty)]
        [(= num (node-key a-bst)) a-bst]
        [(< num (node-key a-bst))
         (make-node (node-key a-bst)
                    (bst-add num (node-left a-bst))
                    (node-right a-bst))]
        [else (make-node (node-key a-bst)
                         (node-left a-bst)
                         (bst-add num (node-right a-bst)))]))

;; Tests
(check-expect (bst-add 3 my-bst) my-bst)
(check-expect (bst-add 4 my-bst)
              (make-node 5
                         (make-node 3 empty (make-node 4 empty empty))
                         (make-node 9 (make-node 7 empty empty) empty)))
(check-expect (bst-add 10 my-bst)
              (make-node 5
                         (make-node 3 empty empty)
                         (make-node 9
                                    (make-node 7 empty empty)
                                    (make-node 10 empty empty))))


;; ==== Question 3c ============================================================

;; (bst-height a-bst) produces the height of a-bst
;; bst-height: BST -> Nat
;; Example:
(check-expect (bst-height empty) 0)

(define (bst-height a-bst)
  (cond [(empty? a-bst) 0]
        [else (+ 1 (max (bst-height (node-left a-bst))
                        (bst-height (node-right a-bst))))]))
;; Tests
(check-expect (bst-height my-bst) 3)
(check-expect (bst-height another-bst) 4)

;; ==== Question 3d ============================================================

;; Trees for tests
(define bst-test1
  (make-node 3
             (make-node 2 (make-node 1 (make-node 0 empty empty) empty) empty)
             (make-node 4 empty (make-node 5 empty empty))))
(define bst-test2
  (make-node 3
             (make-node 2 empty (make-node 1 empty empty))
             (make-node 4 (make-node 5 (make-node 6 empty empty) empty) empty)))

;; (bst-balanced? a-bst) determines whether a-bst is balanced
;; bst-balanced?: BST -> Bool
;; Example:
(check-expect (bst-balanced? empty) true)

(define (bst-balanced? a-bst)
  (cond [(empty? a-bst) true]
        [else (and (<= 0 (abs (- (bst-height (node-left a-bst))
                                 (bst-height (node-right a-bst)))) 1)
                   (bst-balanced? (node-left a-bst))
                   (bst-balanced? (node-right a-bst)))]))

;; Tests
(check-expect (bst-balanced? my-bst) true)
(check-expect (bst-balanced? another-bst) false); fails the first condition in
                                                ; the else case
(check-expect (bst-balanced? bst-test1) false); passed first condition of else 
                                              ; case but left is not balanced
(check-expect (bst-balanced? bst-test2) false); passed first two conditions of
                                              ; else case but right is not
                                              ; balanced




