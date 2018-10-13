#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct aim [ufo tank] #:transparent)
(define-struct fired [ufo tank missile] #:transparent)
(define-struct posn [x y] #:transparent)

; ufo is a posn
; interpretation (make-posn x y) is the ufo location
; left to right, top down convention

(define-struct tank [loc vel] #:transparent)
; tank is a struct
; make-tank number number
; interpretation : make-tank x dx specifice position
; (x, height) and tank speed: dx pixels/tick


; missile is a posn
; interpretation: make-posn x y is the missile place

; a sigs is one of
; - (make-aim ufo tank]
; - (make-fired ufo tank missile]
; interpretation represents the complete state of a space
; invader game

(define W 200)
(define H 200)
(define BACKGROUND (empty-scene W H))
(define TANK-H 10)
(define TANK (rectangle 20 TANK-H "solid" "blue"))

(define UFO (overlay
             (circle 10 "solid" "green")
             (rectangle 30 10 "solid" "green")))

(define MISSILE (triangle 10 "solid" "red"))
(define UV 1)
(define MV 2)

(define s1 (make-aim (make-posn 20 10) (make-tank 28 -3)))
(define s2 (make-fired (make-posn 20 10) (make-tank 28 -3)
                       (make-posn 28 (- H TANK-H))))
(define s3 (make-fired (make-posn 20 100) (make-tank 100 3)
                       (make-posn 22 103)))

; sigs -> image
; add tank ufo and possibly missile to
; the background scene
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s)
                              BACKGROUND))]
    [(fired? s)
     (tank-render (fired-tank s)
                  (ufo-render (fired-ufo s)
                              (missile-render
                               (fired-missile s)
                               BACKGROUND)))]))

; tank image -> image
; add t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) H im))

; ufo image-> image
; add u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; missile image -> image
; add m to the given image im
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

; end?
; sigs -> boolean
(define (si-game-over? si)
  (cond
    [(aim? si)
     (> (posn-y (aim-ufo si)) H)]
    [(fired? si)
     (or (> (posn-y (fired-ufo si)) H)
         (close-enough? (fired-ufo si)
                        (fired-missile si)))]))

(define (close-enough? p1 p2)
  (and
   (< (abs (- (posn-x p1) (posn-x p2))) 5)
   (< (abs (- (posn-y p1) (posn-y p2))) 5)))

; sigs -> sigs
; si-move: tick tock

(define (si-move s)
  (cond
    [(aim? s)
     (make-aim (move-ufo (aim-ufo s))
               (move-tank (aim-tank s)))]
    [(fired? s)
     (if (< (posn-y (fired-missile s)) 0)
         (make-aim
          (move-ufo (fired-ufo s))
          (move-tank (fired-tank s)))
         (make-fired (move-ufo (fired-ufo s))
                     (move-tank (fired-tank s))
                     (move-missile (fired-missile s))))]))

(define (move-ufo u)
  (make-posn (pertube (posn-x u)) (+ (posn-y u) UV)))

(define (pertube y)
  (define r (random 2))
  (define r2 (random 10))
  (define y+ (+ y r2))
  (define y- (- y r2))
  (if (zero? r)
      (ceiling y+ W)
      (floor y- 0)))

(define (ceiling a c)
  (if (>= a c) c a))
(define (floor a f)
  (if (<= a f) f a))

(define (move-tank t)
  (define v (tank-vel t))
  (define v+ (+ (tank-loc t) v))
  (make-tank
   (bounded v+ 0 W) v))

(define (bounded n a b)
  (cond
    [(< n a) a]
    [(> n b) b]
    [else n]))

; y < 0 -> fired -> aim
(define (move-missile m)
  (define y- (- (posn-y m) 5))
  (make-posn
   (posn-x m)
   y-))
;   (if (< y- 0) H y-)))
; sigs -> sigs
; control w key event
(define (si-control s ke)
  (cond
    [(aim? s)
     (cond
       [(string=? ke " ")
        (make-fired
         (aim-ufo s)
         (aim-tank s)
         (make-posn
          (tank-loc (aim-tank s)) 190 ))]
       [(or (string=? ke "left")
            (string=? ke "right"))
        (make-aim (aim-ufo s)
                  (control-tank (aim-tank s) ke))]
       [else s])]
    [(fired? s)
     (cond
       [(or (string=? ke "left")
            (string=? ke "right"))
        (make-fired (fired-ufo s)
                    (control-tank (fired-tank s) ke)
                    (fired-missile s))]
       [else s])]))

(define (control-tank t ke)
  (define v (tank-vel t))
  (make-tank (tank-loc t)
             (cond
               [(string=? ke "left") (- (abs v))]
               [(string=? ke "right") (abs v)]
               [else v])))
;
(define (main s)
  (big-bang s
    [on-tick si-move]
    [on-key si-control]
    [to-draw si-render]
    [stop-when si-game-over?]
      ))