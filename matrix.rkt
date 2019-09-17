;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 06, Q2
;; *****************************************
;;

;; === Data definitions ========================================================

;; A Matrix is a (listof (listof Num))
;; requires: each list of numbers has the same length.
;;           all lists are non-empty

(define M '((1 2 3) (4 5 6) (7 8 9)))
(define M_1 '((1 0) (0 1) (1 1)))

;; ==== Question 2a ============================================================

;; (matrix-row matr pos) produces the row of the matrix matr whose position is
;;   pos if pos < (length matrix), otherwise empty
;; matrix-row: Matrix Nat -> (listof Num)
;; Example:
(check-expect (matrix-row '() 0) '())

(define (matrix-row matrix pos)
  (cond [(empty? matrix) empty]
        [(= 0 pos) (first matrix)]
        [else (matrix-row (rest matrix) (sub1 pos))]))

;; Tests
(check-expect (matrix-row M 0) (list 1 2 3))
(check-expect (matrix-row M 1) (list 4 5 6))
(check-expect (matrix-row M 3) empty)


;; ==== Question 2b ============================================================

;; (matrix-col matrix pos) produces the column in matrix in the position pos if
;;    pos < (length (first matrix)), otherwise empty
;; matrix-col: Matrix Nat -> (listof Num)
;; Example:
(check-expect (matrix-col '() 0) '())

(define (matrix-col matrix pos)
  (cond [(empty? matrix) empty]
        [(>= pos (length (first matrix))) empty]
        [else (cons (list-ref (first matrix) pos)
                    (matrix-col (rest matrix) pos))]))

;; Tests
(check-expect (matrix-col M 3) empty)
(check-expect (matrix-col M 0) (list 1 4 7))
(check-expect (matrix-col M 1) (list 2 5 8))
(check-expect (matrix-col M 2) (list 3 6 9))


;; ==== Question 2c ============================================================

;; (get-element matrix row-pos col-pos) produces the elemnent in matrix
;;   that is in the row-pos-th row and the col-pos-th column
;; get-element: Matrix Nat Nat -> Num
;; requires: matrix to be non-empty
;;           row-pos < (length matrix)
;;           col-pos < (length (first matrix))
;; Example:
(check-expect (get-element M 0 0) 1)

(define (get-element matrix row column)
  (list-ref (matrix-row matrix row) column))

;; Tests
(check-expect (get-element M 1 1) 5)
(check-expect (get-element M 1 2) 6)

;; ==== Question 2d ============================================================

;; (row-add r1 r2) produces the list obtained by adding the corresponding
;;   numbers of the rows r1 and r2
;; row-add: (listof Num) (listof Num) -> (listof Num)
;; requires: r1 and r2 are the same size
;; Examples:
(check-expect (row-add '() '()) '())
(check-expect (row-add '(1 2) '(1 1)) '(2 3))

(define (row-add r1 r2)
  (cond [(empty? r1) empty]
        [else (cons (+ (first r1) (first r2)) (row-add (rest r1) (rest r2)))]))


;; (matrix-add m1 m2) produces the addition of the matrices m1 and m2
;; matrix-add: Matrix Matrix -> Matrix
;; requires: m1 and m2 have the same number of rows and columns
;; Example:
(check-expect (matrix-add '() '()) '())

(define (matrix-add m1 m2)
  (cond [(empty? m1) empty]
        [else (cons (row-add (first m1) (first m2))
                    (matrix-add (rest m1) (rest m2)))]))

;; Tests
(check-expect (matrix-add '((1) (3)) '((2) (4))) '((3) (7)))
(check-expect (matrix-add '((1 2))'((2 1))) '((3 3)))
(check-expect (matrix-add '((1 2) (3 4)) '((1 0) (0 1))) '((2 2) (3 5)))


;; ==== Question 2e ============================================================

;; (dot-product lon1 lon2) produces the sum of the products of the corresponding
;;   elements of the lists lon1 and lon2
;; dot-product: (listof Num) (listof Num) -> Num
;; requires: lon1 and lon2 to have the same size
;; Examples:
(check-expect (dot-product '() '()) 0)
(check-expect (dot-product '(1) '(2)) 2)

(define (dot-product lon1 lon2)
  (cond [(empty? lon1) 0]
        [else (+ (* (first lon1) (first lon2))
                 (dot-product (rest lon1) (rest lon2)))]))

;; (matrix-multiply/row-acc row1 matr acc) produces a list with the elements in
;;   the product of row1 and matr from the acc-th row to the last one
;; matrix-multiply/row-acc: (listof Num) Matrix Nat -> (list Nat)
;; requires: row1 and matr are the same size
;;           matr is non-empty
;;           (length (first matr)) > acc
;; Example:
(check-expect (matrix-multiply/row-acc '(1 2) '((1) (1)) 0) '(3))

(define (matrix-multiply/row-acc row1 matr acc)
  (cond [(= (length (first matr)) (add1 acc))
         (cons (dot-product row1 (matrix-col matr acc)) empty)]
        [else (cons (dot-product row1 (matrix-col matr acc))
                    (matrix-multiply/row-acc row1 matr (add1 acc)))]))

;; (matrix-multiply/row row matr) produces the product of row and matr
;; matrix-multiply/row: (listof Num) Matrix -> (list Nat)
;; requires: row and matr are the same size and non-empty
;; Example:
(check-expect (matrix-multiply/row '(1 2) '((2) (2))) '(6))

(define (matrix-multiply/row row matr)
  (matrix-multiply/row-acc row matr 0))

;; (matrix-multiply row matr) produces the multiplication of matr1 and matr2
;; matrix-multiply: Matrix Matrix -> Matrix
;; requires: matr1 and matr2 have the same number of rows and columns
;; Example:
(check-expect (matrix-multiply '() '()) '())

(define (matrix-multiply m1 m2)
  (cond [(empty? m1) empty]
        [else (cons (matrix-multiply/row (first m1) m2)
                    (matrix-multiply (rest m1) m2))]))

;; Tests
(check-expect (matrix-multiply '((1 2)) '((2) (2))) '((6)))
(check-expect (matrix-multiply '((1 2) (3 4)) '((2 1) (4 3))) '((10 7) (22 15)))
(check-expect (matrix-multiply M M_1) '((4 5) (10 11) (16 17)))
