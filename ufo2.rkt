#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct ufo [posn vel])
(define-struct posn [x y])
(define-struct vel [deltax deltay])
; a ufo is a structure
; (make-ufo posn vel)
; interpretation (make-ufo p v) is at location
; p moving at velocity v

(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))
(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))
(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; ufo -> ufo
; determines where u moves in one clock tick
; leave the velocity as is

(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2)
              (make-ufo (make-posn 17 77) v2))
(define (ufo-move-1 u) 
  (make-ufo (posn+ (ufo-posn u) (ufo-vel u))
            (ufo-vel u)))

; posn vel -> posn
; add v to p
(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))

(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))

(define W 300)
(define H 100)
(define C (empty-scene W H))
(define UFO (overlay (circle 10 "solid" "green")
                     (rectangle 30 10 "solid" "green")))


; ufo -> image
(define (render ufo)
  (define loc (ufo-posn ufo))
  (place-image UFO (posn-x loc) (posn-y loc) C))

(check-expect (render u1)
              (place-image UFO (posn-x (ufo-posn u1)) (posn-y (ufo-posn u1)) C))

; ufo -> ufo
(define (main u0)
  (big-bang u0
            [on-tick ufo-move-1]
            [to-draw render]))
              
