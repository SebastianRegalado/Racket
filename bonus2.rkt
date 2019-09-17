;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (encode-msg-1 los1 los2 a b c) produces the Decryptor needed
;;   to hide the los2 in los1
;; encode-msg-1: (listof Sym) (listof Sym) (Anyof Nat false) 
;;               (Anyof Nat false) (Anyof Nat false) -> (list Nat Nat Nat) 
;; requires: los2 to be a 3-elements-ordered sub-list of los1
;; Example: 
(check-expect (encode-msg-1 (list #\a #\b #\a) (list #\a #\b #\a) 0
                            false false)
              (list 0 0 0))

(define (encode-msg-1 los1 los2 a b c)
  (cond [(boolean? c)
         (cond [(boolean? b)
                (cond [(equal? (first los1) (first los2))
                       (encode-msg-1 (rest los1) (rest los2) a 0 c)]
                      [else (encode-msg-1 (rest los1) los2 (add1 a) b c)])]
               [(equal? (first los1) (first los2))
                (encode-msg-1 (rest los1) (rest los2) a b 0)]
               [else (encode-msg-1 (rest los1) los2 a (add1 b) c)])]
        [(equal? (first los1) (first los2)) (list a b c)]
        [else (encode-msg-1 (rest los1) los2 a b (add1 c))]))


;; (encode-msg sring1 string2) produces the Decryptor needed to hide
;;   string2 in string1
;; encode-msg-1: Str Str -> (list Nat Nat Nat) 
;; requires: there exists at least one such Decryptor
;; Example:
(check-expect (encode-msg "abcdefg" "abc") (list 0 0 0))

(define (encode-msg string1 string2)
  (encode-msg-1 (string->list string1) (string->list string2) 0 false false))

;; Tests
(check-expect (encode-msg "abcdefg" "beg") (list 1 2 1))
(check-expect (encode-msg "abcdefg" "ace") (list 0 1 1))
(check-expect (encode-msg "abcdefg" "ace") (list 0 1 1))

