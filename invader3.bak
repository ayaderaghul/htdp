#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct sigs [ufo tank missile] #:transparent)
(define-struct posn [x y] #:transparent)
(define-struct tank [loc vel] #:transparent)
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

(define s1 (make-sigs (make-posn 20 10) (make-tank 28 -3) #f))
(define s2 (make-sigs (make-posn 20 10) (make-tank 28 -3)
                       (make-posn 28 (- H TANK-H))))
(define s3 (make-sigs (make-posn 20 100) (make-tank 100 3)
                       (make-posn 22 103)))


; a sigs 2 is a structure
; (make-sigs ufo tank missileornot]
; interpretation represents the complete state of a
; space invader game

; a missileornot is one of:
; - #f
; - posn
; interpretation #f means the missile in in tank
; posn says that missile is at that location

; missileornot image -> image
; add an image of missile m to scene s
(define (missile-render m s)
  (cond
    [(boolean? m) s]
    [(posn? m) (place-image MISSILE (posn-x m)
                            (posn-y m) s)]))
; tank image -> image
; add t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) H im))

; ufo image-> image
; add u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

(define (render s)
  (ufo-render (sigs-ufo s)
              (tank-render (sigs-tank s)
                           (missile-render (sigs-missile s)
                                           BACKGROUND))))

(define (over? s)
  (if (boolean? (sigs-missile s))
      (> (posn-y (sigs-ufo s)) H)
      (or
       (> (posn-y (sigs-ufo s)) H)
       (close-enough? (sigs-ufo s)
                      (sigs-missile s)))))
(define (close-enough? p1 p2)
  (and
   (< (abs (- (posn-x p1) (posn-x p2))) 5)
   (< (abs (- (posn-y p1) (posn-y p2))) 5)))


(define (move s)
  (make-sigs
   (move-ufo (sigs-ufo s))
   (move-tank (sigs-tank s))
   (if (or (boolean? (sigs-missile s))
           (< (posn-y (sigs-missile s)) 0))
       #f
       (move-missile (sigs-missile s)))))

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
(define (move-missile m)
  (define y- (- (posn-y m) 5))
  (make-posn
   (posn-x m)
   y-))


(define (control s ke)
  (make-sigs
   (sigs-ufo s)
   (cond
     [(or (string=? ke "left")
          (string=? ke "right"))
      (control-tank (sigs-tank s) ke)]
     [else (sigs-tank s)])
   (cond
     [(and (string=? ke " ")
           (boolean? (sigs-missile s)))
      (make-posn (tank-loc (sigs-tank s)) H)]
     [else (sigs-missile s)])))

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
    [on-tick move]
    [on-key control]
    [to-draw render]
    [stop-when over?]
      ))