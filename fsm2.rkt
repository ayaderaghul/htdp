#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct ktransition [current key next])
; atransition is a structure:
; (make-ktransition fsm-state keyevent fsm-state)

(define regex
  (list (make-ktransition "begin" '("a") "a")
        (make-ktransition "a" '("b" "c") "a")
        (make-ktransition "a" '("d") "d")
        (make-ktransition "a" "notbcd" "error")))

(define-struct fsm [fs current])

(define (render f)
  (define cur (fsm-current f))
  (cond
    [(string=? cur "begin") (empty-image)]
    [(string=? cur "a") (square 100 "solid" "white")]
    [(string=? cur "d") (square 100 "solid" "green")]
    [(string=? cur "error") (square 100 "solid" "red")]))

(define (find-next-state f cur ke)
  (cond
    [(


(define (simulate an-fsm s0)
  (big-bang (make-fsm an-fsm s0)
            [to-draw render]
            [on-key find-next-state]))
                     

