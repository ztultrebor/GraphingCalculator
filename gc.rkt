;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Graph a function


; ========================
; constants

(define WIDTH 400)
(define HEIGHT 300)
(define POINT (circle 3 "solid" "blue"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))


; ========================
; data definitions

(define-struct function [graphed-x graphed-y to-be-graphed-x to-be-graphed-y])
; a Function is a [[ListOf Number] [ListOf Number]
;                  [ListOf Number] [ListOf Number]]
#; 
(define (fn-on-function f)
  (...(first (function-graphed-x f)) ... (first (function-graphed-y f))
      ... (first (function-to-be-graphed-x f))
      ... (first (function-to-be-graphed-y f))
      ... (make-function (rest graphed-x) (rest graphed-y)
                         (rest to-be-graphed-x) (rest to-be-graphed-x))))


; ========================
; functions

(define (main func x)
  ; Game -> Game
  ; run the pocket universe
  (local (
          (define fx (map func x))
          (define minx (minimum x))
          (define maxx (maximum x))
          (define minfx (minimum fx))
          (define maxfx (maximum fx))
          (define xrange
            (map (lambda (n) (* (/ WIDTH (- maxx minx)) (- n minx))) x))
          (define yrange
            (map (lambda (n) (* (/ HEIGHT (- minfx maxfx)) (- n maxfx))) fx))
          (define data (make-function
                        (list (first xrange)) (list (first yrange))
                        (rest xrange) (rest yrange))))
    ; - IN -
    (big-bang data
      [on-tick graph 1/112]
      [to-draw render]
      [stop-when end?])))


(define (graph d)
  ; Function -> Function
  ; updates the current point of graphing
  (make-function (cons (first (function-to-be-graphed-x d))
                       (function-graphed-x d))
                 (cons (first (function-to-be-graphed-y d))
                       (function-graphed-y d))
                 (rest (function-to-be-graphed-x d))
                 (rest (function-to-be-graphed-y d))))


(define (render d)
  ; Function -> Image
  ; takes a function and a domain of numbers and returns the
  ; range of the applied function
  (local (
          (define xrng (function-graphed-x d))
          (define yrng (function-graphed-y d))
          (define (draw-segment x y)
            ; [ListOf Number] [ListOf Number] -> Image
            ; draw each tiny line segment of a curve in succession
             (cond
               [(empty? x) BACKGROUND]
               [(empty? (rest x)) BACKGROUND]
               [else
                (add-line (draw-segment (rest x) (rest y))
                          (first x) (first y) (second x) (second y) "blue")])))
    ; - IN -
    (place-image POINT (first xrng) (first yrng) (draw-segment xrng yrng))))


(define (end? d)
  ; Function -> Boolean
  ; stops when function is completely graphed
  (= 0 (length (function-to-be-graphed-x d))))
          

(define (maximum lon)
  ; [ListOf Number] -> Number
  ; returns the maximum of a list of numbers
  (foldr (lambda (a b) (if (> a b) a b)) (first lon) lon))


(define (minimum lon)
  ; [ListOf Number] -> Number
  ; returns the minimum of a list of numbers
  (foldr (lambda (a b) (if (< a b) a b)) (first lon) lon))


; =======================
; checks

(define LONNY (build-list 10 (lambda (x) x)))
(check-expect (maximum LONNY) 9)
(check-expect (minimum LONNY) 0)


; =======================
; action!

;(main (lambda (x) (sin x)) (range 0 10 0.01))

;(main (lambda (x) (exp x)) (range -5 5 0.01))

(main (lambda (x) (+ (* (sqr x) (sqr x)) (- x) -1)) (range -2 2 0.005))