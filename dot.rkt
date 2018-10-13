#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct posn (x y))
(define C (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; a posn represents the state of the world

; posn -> posn 
(define (main p0)
  (big-bang p0
            [on-tick x+]
            [on-mouse reset-dot]
            [to-draw scene+dot]))

; posn -> image
; add a red dot to canvas at p
(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p)
  C))
(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 C))
(check-expect (scene+dot (make-posn 88 73))
              (place-image DOT 88 73 C))

; posn->posn
; increases the x coor of p by 3
(check-expect (x+ (make-posn 10 0)) (make-posn 13 0))
(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

; posn number number mouseevt-> posn
; for mouse clicks, (make-posn x y), else p
(define (reset-dot p x y me) 
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))

(check-expect (reset-dot (make-posn 10 20) 29 31 "button-down")
              (make-posn 29 31))
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-up")
              (make-posn 10 20))

