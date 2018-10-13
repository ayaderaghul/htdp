#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

; TrafficLight -> TrafficLight
; yields the next state given current state s
(define R 50) ; radius of bulb

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

(define (posn s)
  (cond
    [(string=? "red" s) 50]
    [(string=? "green" s) 250]
    [(string=? "yellow" s) 150]))
   

(define W 100)
(define H 300)
(define C (empty-scene W H))

(define red (circle 50 "solid" "red"))
(define green (circle 50 "solid" "green"))
(define yellow (circle 50 "solid" "yellow"))

; state: color

; clock tick handler
(define (tl-next x)
  (traffic-light-next x))

(define (end? x)
  (= x 100))



; render
(define (tl-render x)
  (define new-posn (posn x))
  (place-image (circle 50 "solid" x) 50 new-posn C))

(check-expect (tl-render "red") 
              (place-image red 50 50 C))
(check-expect (tl-render "yellow")
              (place-image yellow 50 150 C))
(check-expect (tl-render "green")
              (place-image green 50 250 C))


; trafficlight -> trafficlight
; simulates a clock based american traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [on-tick tl-next 1 10]
            [to-draw tl-render]))
            ;[stop-when zero?]))
