;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 05, Question 4
;; *****************************************
;;


;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; requires: each Nat is either 0 or 1

;; A Rating is an Int
;; requires: Int is either -1 or 1

(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)

;; A Pref-v is a (list Int Int Int Int Int Int Int Int)

;; ==== Question 4a ============================================================


;; (add-list lon1 lon2) produces a list of numbers formed by adding the
;;   corresponding elements of the lists of numbers lon1 and lon2
;; add-list: (listof Num) (listof Num) -> (listof Num)
;; requires: lon1 and lon2 to have the same size
;; Example:
(check-expect (add-list (list 1 2) (list 1 2)) (list 2 4))

(define (add-list lon1 lon2)
  (cond [(empty? lon1) empty]
        [else (cons (+ (first lon1) (first lon2))
                    (add-list (rest lon1) (rest lon2)))]))

;; (negate-list lon) produces a list of numbers formed by multiplying each of
;;   the numbers in lon by -1
;; negate-list: (listof Num) -> (listof Num)
;; Example:
(check-expect (negate-list (list 1 2)) (list -1 -2))

(define (negate-list lon)
  (cond [(empty? lon) empty]
        [else (cons (* -1 (first lon)) (negate-list (rest lon)))]))

;; (preference-per-movie num movie-fv) produces the Pref-v of a
;;   client corresponding to one movie given its rating num and movie-fv
;; preference-per-movie: (Anyof 1 -1) Movie-fv -> Pref-v
;; Example:
(check-expect (preference-per-movie 1 (list 1 1 1 0 0 0 0 0))
              (list 1 1 1 0 0 0 0 0))

(define (preference-per-movie num movie-fv)
  (cond [(= num 1) movie-fv]
        [else (negate-list movie-fv)]))

;; (find-preference lo-ratings movie-fvectors) produces the Pref-v of a
;;   client corresponding to their lo-ratings and the movie-fvectors of the
;;   movies he or she watched
;; find-preference: (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: lo-ratings and movie-fvectors non-empty and have the same size
;; Example:
(check-expect (find-preference (list 1)
                               (list (list 1 1 1 0 0 0 0 0)))
              (list 1 1 1 0 0 0 0 0))

(define (find-preference lo-ratings movie-fvectors)
  (cond [(empty? (rest lo-ratings))
         (preference-per-movie (first lo-ratings) (first movie-fvectors))]
        [else
         (add-list (preference-per-movie (first lo-ratings)
                                         (first movie-fvectors))
                   (find-preference (rest lo-ratings) (rest movie-fvectors)))]))

;; Tests
(check-expect (find-preference (list -1)
                               (list (list 1 1 1 0 0 0 0 0)))
              (list -1 -1 -1 0 0 0 0 0))
(check-expect (find-preference (list 1 1 -1)
                               (list (list 1 1 0 0 0 0 0 0)
                                     (list 1 1 1 0 0 0 0 0)
                                     (list 0 1 1 1 0 0 0 0)))
              (list 2 1 0 -1 0 0 0 0))

;; ==== Question 4b ============================================================

;; (dot-product lon1 lon2) produces the sum of the products of the corresponding
;;   elements of the lists lon1 and lon2
;; dot-product: (listof Num) (listof Num) -> Num
;; requires: lon1 and lon2 to have the same size
;; Example:
(check-expect (dot-product (list 1) (list 2)) 2)

(define (dot-product lon1 lon2)
  (cond [(empty? lon1) 0]
        [else (+ (* (first lon1) (first lon2))
                 (dot-product (rest lon1) (rest lon2)))]))

;; (suggested-movie pref-v lomovies) produces a Movie in lomovies that has
;;   the generates the greatest dot product between itself and pref-v
;; suggested-movie: Pref-v (listof Movie) -> Movie
;; requires: lomovies to be non-empty
;; Example:
(check-expect (suggested-movie
               (list 2 1 0 -1 0 0 0 0)
               (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              (make-movie "The Meg" (list 1 0 0 0 1 0 1 0)))

(define (suggested-movie pref-v lomovies)
  (cond [(empty? (rest lomovies)) (first lomovies)]
        [(> (dot-product pref-v (movie-genres (first lomovies)))
            (dot-product pref-v (movie-genres
                                 (suggested-movie pref-v (rest lomovies)))))
         (first lomovies)]
        [else (suggested-movie pref-v (rest lomovies))]))

;; (suggestions pref-v lomovies) produces a title of the movie in lomovies that 
;;   has the generates the greatest dot product between itself and pref-v
;; suggestions: Pref-v (listof Movie) -> Str
;; requires: lomovies to be non-empty
;; Example:
(check-expect (suggestions
               (list 2 1 0 -1 0 0 0 0)
               (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              "The Meg")

(define (suggestions pref-v lomovies)
  (movie-title (suggested-movie pref-v lomovies)))

;; Tests
(check-expect (suggestions
               (list 2 1 0 -1 0 0 0 0)
               (list (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                     (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              "The Meg")
(check-expect (suggestions
               (list 2 1 0 -1 0 0 0 0)
               (list (make-movie "Smallfoot" (list 1 1 1 1 0 0 0 0))
                     (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              "The Meg")




              