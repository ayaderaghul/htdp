#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define H 80)
(define W 100)
(define XSHOTS (/ W 2))
(define B (empty-scene W H))
(define SHOT (triangle 3 "solid" "red"))

; a list of shots is one of:
; '()
; (cons shot los)
; interpretation the collection of shot fired


; shot is a number
; interpretation represents the shot's y coor

; a shot world is a list of numbers
; interpretatino each number on such a list
; represents the y coor of a shot

; shotworld -> image
; add image of a shot for each y on w
; at (mid, y) to the background image
(define (to-image w)
  (cond
    [(empty? w)B]
    [else
     (place-image SHOT XSHOTS (first w)
                  (to-image (rest w)))]))


(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 B))




; shotw -> shotw
; moves each shot on w up by one pixel
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (if (>= (first w) 1)
              (cons (sub1 (first w)) (tock (rest w)))
              (rest w))]))

(check-expect (tock (cons 3 '()))
              (cons 2))


; shotw keyevent -> shotw
; add a shot to the world
; if the player presses the space bar
(define (keyh w ke)
  (if (key=? ke " ") (cons H w) w))

(check-expect (keyh (cons 3 '()) " ")
              (cons H (cons 3 '())))

; shotw -> shotw
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))