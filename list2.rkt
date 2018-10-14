#lang racket
(require test-engine/racket-tests)

(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

(define (my-add a b)
  (cond
    [(zero? a) b]
    [else (add1 (my-add (sub1 a) b))]))
(check-expect (my-add 2 3) 5)

(define (my-mult a b)
  (cond
    [(= 1 a) b]
    [else (+ b (my-mult (sub1 a) b))]))
(check-expect (my-mult 3 5) 15)




(require 2htdp/image 2htdp/universe)

(define W 80)
(define H 180)
(define B (empty-scene W H))
(define S 10)
(define SQ (square S 'outline "black"))

(define (place-col img n b)
  (place-image/align img 0 (* n S) "left" "top" b))

(define (col n img)
 (cond
   [(zero? n) (place-col img n B)]
   [else (place-col img n (col (sub1 n) img))]))

(define (place-row img n b)
  (place-image/align img (* n S) 0 "left" "top" b))

(define (row n img)
  (cond
    [(zero? n) (place-row img n B)]
    [else (place-row img n (row (sub1 n) img))]))

;;;;

(define-struct posn (x y) #:transparent)
(define (origin? p)
  (and (zero? (posn-x p)) (zero? (posn-y p))))

(define (place-at img p b)
  (place-image/align img (* (posn-x p) S) (* (posn-y p) S) "left" "top" b))

(define (back1 p ym)
  (define x (posn-x p))
  (define y (posn-y p))
  (if (zero? y)
      (make-posn (sub1 x) ym)
      (make-posn x (sub1 y))))

(check-expect (back1 (make-posn 7 17) 17) (make-posn 7 16))
(check-expect (back1 (make-posn 7 0) 17) (make-posn 6 17))

(define HW 8)
(define HH 18)
(define O (make-posn 0 0))
(define M (make-posn 7 17))

(define (hall p img)
 (cond
   [(origin? p) (place-at img p B)]
   [else (place-at img p (hall (back1 p (sub1 HH)) img))]))

; add balloons
; lop -> img

(define BALL (circle 5 "solid" "red"))
(define (generate-balls n)
  (cond
    [(zero? n) '()]
    [else (cons (make-posn (* (random HW) S) (* (random HH) S)) (generate-balls (sub1 n)))]))
(define LOB (generate-balls 6))

(define (add-balloons lob)
  (cond
    [(empty? (rest lob)) (place-image BALL (posn-x (first lob)) (posn-y (first lob))
                                      (hall M SQ))]
    [else (place-image BALL (posn-x (first lob)) (posn-y (first lob))
                       (add-balloons (rest lob)))]))
    
(add-balloons LOB)

;; rd -> number
; how many dolls
(define-struct layer [color doll])
(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ (depth (layer-doll an-rd)) 1)]))

(check-expect (depth "red") 1)
(check-expect (depth (make-layer "yellow" (make-layer "green" "red"))) 3)


;; rd -> string
; list all colors
(define (list-out rd)
  (cond
    [(string? rd) rd]
    [else
     (string-append (layer-color rd) ", " (list-out (layer-doll rd)))]))
(check-expect (list-out "red") "red")
(check-expect (list-out (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")

(define (innermost rd)
  (cond
    [(string? rd) rd]
    [else
     (innermost (layer-doll rd))]))
(check-expect (innermost "red") "red")
(check-expect (innermost (make-layer "yellow" (make-layer "green" "red")))
              "red")

(test)