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
; functions

(define (graph func x)
  ; [Number -> Number] [ListOf Number] -> [ListOf Number]
  ; takes a function and a domain of numbers and returns the
  ; range of the applied function
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
          (define (draw-segment x y)
            ; [ListOf Number] [ListOf Number] -> Image
            ; draw each tiny line segment of a curve in succession
            (cond
              [(empty? (rest x)) BACKGROUND]
              [else
               (add-line (draw-segment (rest x) (rest y))
                         (first x) (first y) (second x) (second y) "blue")])))
    ; - IN -
    (draw-segment xrange yrange)))
          

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

(graph (lambda (x) (sin x)) (range 0 10 0.001))