#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define W 80)
(define H 180)
(define B (empty-scene W H))
(define S 10)
(define SQ (square S 'outline "black"))

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

;;;;;;;;;;


(define-struct pair (n lob) #:transparent)
; a pair is a struct (make-pair n lop)
; a lop is one of:
; '()
; (cons posn lop}
; interpretation: (make-pair n lob} means n balloons
; to be thrown and added to lob

(define (main n)
  (big-bang (throw (make-pair n '()))
    [on-tick throw 1]
    [to-draw to-image]
    [stop-when done?]))

;; throw
; pair -> pair
;

(define (throw p)
  (make-pair
   (sub1 (pair-n p))
   (cons (make-posn (* (random HW) S) (* (random HH) S))
         (pair-lob p))))

; to-image
; pair -> image
;
(define (to-image p)
  (cond
    [(empty? (rest (pair-lob p)))
     (place-image BALL
                  (posn-x (first (pair-lob p)))
                  (posn-y (first (pair-lob p)))
                  (hall M SQ))]
    [else
     (place-image BALL
                  (posn-x (first (pair-lob p)))
                  (posn-y (first (pair-lob p)))
                  (to-image (make-pair
                             (pair-n p)
                             (rest (pair-lob p)))))]))

; done?
; pair -> boolean
(define (done? p)
  (zero? (pair-n p)))