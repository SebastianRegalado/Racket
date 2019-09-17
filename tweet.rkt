;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tweet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 05, Question 3
;; *****************************************
;;

(define-struct tweet (sender text tags))
;; A Tweet is a (make-tweet Str Str (listof Sym))
;; requires: tags does not contain any duplicate tags


;; ==== Question 3a ============================================================

;; (symbol-insert symbol los) insert symbol at the end of los if it was not
;;   already in los, and returns los otherwise
;; symbol-insert: Sym (listof Sym) -> (listof Sym) 
;; Example:
(check-expect (symbol-insert 'Hola (list 'Ciao 'Hi)) (list 'Ciao 'Hi 'Hola))

(define (symbol-insert symbol los)
  (cond [(empty? los) (list symbol)]
        [(symbol=? symbol (first los)) los]
        [else (cons (first los) (symbol-insert symbol (rest los)))]))


;; (retweet tweet new-sender tag) produces a Tweet formed by replacing tweet's
;;   sender by new-sender and adding tag to the tags field of tweet 
;; retweet: Tweet Str (Anyof Sym false) -> Tweet
;; Example:
(check-expect (retweet (make-tweet "user1" "test tweet" (list 'a 'b))
                       "user2" 'c)
              (make-tweet "user2" "test tweet" (list 'a 'b 'c)))

(define (retweet tweet sender tag)
  (cond [(boolean? tag)
         (make-tweet sender (tweet-text tweet) (tweet-tags tweet))]
        [else (make-tweet sender
                          (tweet-text tweet)
                          (symbol-insert tag (tweet-tags tweet)))]))

;; Tests
(check-expect (retweet (make-tweet "user1" "test tweet" (list 'a 'b))
                       "user2" 'b)
              (make-tweet "user2" "test tweet" (list 'a 'b)))
(check-expect (retweet (make-tweet "user1" "test tweet" (list 'a 'b))
                       "user2" false)
              (make-tweet "user2" "test tweet" (list 'a 'b)))


;; ==== Question 3b ============================================================


;; (find-rt lotweets string) produces the list of Tweets that are in lotweets
;;   whose text fields equal string 
;; find-rt: (listof Tweet) Str -> (listof Tweet)
;; Example:
(check-expect (find-rt (list (make-tweet "user1" "C" empty)) "A") empty)

(define (find-rt lotweets string)
  (cond [(empty? lotweets) empty]
        [(string=? string (tweet-text (first lotweets)))
         (cons (first lotweets) (find-rt (rest lotweets) string))]
        [else (find-rt (rest lotweets) string)]))

;; Tests
(check-expect (find-rt empty "A") empty)
(check-expect (find-rt (list (make-tweet "user1" "A" empty)
                             (make-tweet "user2" "B" empty)
                             (make-tweet "user2" "A" empty))
                       "A")
              (list (make-tweet "user1" "A" empty)
                    (make-tweet "user2" "A" empty)))
(check-expect (find-rt (list (make-tweet "user1" "A" empty)
                             (make-tweet "user2" "B" empty)
                             (make-tweet "user2" "C" empty))
                       "A")
              (list (make-tweet "user1" "A" empty)))

;; ==== Question 3c ============================================================


;; (find-tagged lotweets symbol) produces the list of Tweets that are in
;;   lotweets whose tags fields contain symbol 
;; find-tagged: (listof Tweet) Sym -> (listof Tweet)
;; Example:
(check-expect (find-tagged (list (make-tweet "u1" "C" (list 'a 'b))) 'c) empty)

(define (find-tagged lotweets symbol)
  (cond [(empty? lotweets) empty]
        [(member? symbol (tweet-tags (first lotweets)))
         (cons (first lotweets) (find-tagged (rest lotweets) symbol))]
        [else (find-tagged (rest lotweets) symbol)]))

;; Tests
(check-expect (find-tagged empty 'c) empty)
(check-expect (find-tagged (list (make-tweet "u1" "C" (list 'a 'b))
                                 (make-tweet "u1" "A" (list 'c))
                                 (make-tweet "u2" "T" (list 'a 'b))
                                 (make-tweet "u3" "!" (list 'a 'c)))
                           'c)
              (list (make-tweet "u1" "A" (list 'c))
                    (make-tweet "u3" "!" (list 'a 'c))))
(check-expect (find-tagged (list (make-tweet "u1" "C" (list 'a 'b))
                                 (make-tweet "u3" "!" (list 'a 'c)))
                           'b)
              (list (make-tweet "u1" "C" (list 'a 'b))))

;; ==== Question 3d ============================================================


;; (find-tagged/multi lotweets los) produces the list of Tweets that are in
;;   lotweets whose tags field contain all the elements in los 
;; find-tagged/multi: (listof Tweet) (listof Sym) -> (listof Tweet)
;; Example:
(check-expect (find-tagged/multi (list (make-tweet "u" "C" (list 'a)))
                                 (list 'a))
              (list (make-tweet "u" "C" (list 'a))))

(define (find-tagged/multi lotweets los)
  (cond [(empty? lotweets) empty]
        [(empty? los) lotweets]
        [else
         (find-tagged/multi (find-tagged lotweets (first los)) (rest los))]))

;; Tests
(check-expect (find-tagged/multi empty (list 'a)) empty)
(check-expect (find-tagged/multi (list (make-tweet "u" "C" (list 'a))) empty)
                                 (list (make-tweet "u" "C" (list 'a))))
(check-expect (find-tagged/multi (list (make-tweet "u" "C" (list 'a))
                                       (make-tweet "u" "A" (list 'b))
                                       (make-tweet "u" "T"
                                                   (list 'c 'a 'b)))
                                 (list 'a 'b))
              (list (make-tweet "u" "T" (list 'c 'a 'b))))
(check-expect (find-tagged/multi (list (make-tweet "u" "C" (list 'a))
                                       (make-tweet "u" "A" (list 'b))
                                       (make-tweet "u" "T"
                                                   (list 'c 'a 'b)))
                                 (list 'a 'b 'd))
              empty)