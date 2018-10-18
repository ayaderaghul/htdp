#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define W 200)
(define H 200)
(define (B color) (empty-scene W H color))

(define-struct regex [current-state] #:transparent)
(define r0 (make-regex "begin"))
(define (render r)
  (define c (regex-current-state r))
  (cond
    [(string=? c "begin") (B "white")]
    [(string=? c "a") (B "yellow")]
    [(string=? c "d") (B "green")]
    [(string=? c "nbcd") (B "red")]))

(define (change r ke)
  (define c (regex-current-state r))
  (cond
    [(string=? c "begin")
     (if (string=? ke "a") (make-regex "a") r)]
    [(string=? c "a")
     (cond
       [(or (string=? ke "b") (string=? ke "c")) r]
       [(string=? ke "d") (make-regex "d")]
       [else (make-regex "nbcd")])]
    [(or (string=? c "nbcd") (string=? c "d"))
     (make-regex "begin")]))
  
(define (main r0)
  (big-bang r0
            [on-key change]
            [to-draw render]))
            