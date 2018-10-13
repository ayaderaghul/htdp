#lang racket
(require 2htdp/image 2htdp/universe)

(define W 400)
(define H 100)
(define WORLD (empty-scene W H))

;(define BAR
;  (rectangle 300 20 "solid" "red"))
(define (HAPPINESS h)
  (rectangle h 20 "solid" "red"))

;(define (H-BAR h)
;  (overlay/align "left" "top" (HAPPINESS h) BAR))

(define (tock x)
  (if (zero? x) 
      0
      (sub1 x)))

(define (render x)
  (define new-happiness (tock x))
  (define new-bar (rectangle new-happiness 20 "solid" "red"))
  (place-image/align new-bar 10 10 "left" "top" WORLD))

(define (pet x ke)
  (cond 
    [(string=? ke "up")
      (+ x (/ x 3))]
    [(string=? ke "down")
      (+ x (/ x 5))]
    [else x]))

(define (end? x)
  (equal? x 0))

(define (main ws)
  (big-bang ws
            [on-tick tock]
            [on-key pet]
            [to-draw render]
            [stop-when zero?]
            ))
   