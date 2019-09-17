;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname prim-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 07, Q3a (bonus)
;; *****************************************
;;

(require "a07drawinglib.rkt")

(define prim-picture
  (list
   (make-prim-circle (make-posn 130 100) 20 "Brown")
   (make-prim-circle (make-posn 142 135) 17 "Brown")
   (make-prim-circle (make-posn 160 159) 13 "Brown")
   (make-prim-circle (make-posn 167 183) 12 "Brown")
   (make-prim-circle (make-posn 179 199) 8 "Brown")
   (make-prim-triangle
    (make-posn 146 149) (make-posn 200 30) (make-posn 280 130) "Orange")
   (make-prim-triangle
    (make-posn 146 149) (make-posn 275 140) (make-posn 240 220) "Orange")
   (make-prim-triangle
    (make-posn 140 153) (make-posn 25 50) (make-posn 5 195) "Orange")
   (make-prim-triangle
    (make-posn 140 153) (make-posn 35 210) (make-posn 140 260) "Orange")))

;;(save-image (render-image (make-posn 300 300) prim-picture) "prim-bonus.png")


