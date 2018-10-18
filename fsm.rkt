#lang racket

(require 2htdp/image 2htdp/universe test-engine/racket-tests)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes 

(define (state? x)
  (string=? x (or "red" "green" "yellow")))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

(define-struct fs [fsm current])
; a simulationstate is a structure
; (make-fs fsm fsm-state)


(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

(define (render an-bw)
  (square 100 "solid" (fs-current an-bw)))

; simulationstate -> image
; renders current world state as a colored square

(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

(check-expect (render (make-fs bw "black"))
              (square 100 "solid" "black"))

; simulationstate keyevent -> simulationstate
; finds the next state from ke and cs


(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))

(check-expect
 (find-next-state (make-fs fsm-traffic "green") "q")
 (make-fs fsm-traffic "yellow"))

(check-expect
 (find-next-state (make-fs bw "black") "q")
 (make-fs bw "white"))


(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm))))

; fsm fsm-state -> fsm-state
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "black") "not found: black")

(check-expect (find bw "back") "white")

(define (find transitions current)
  (cond
    [(empty? transitions) (string-append "not found: " current)]
    [else
     (if (string=? (transition-current (first transitions)) current)
         (transition-next (first transitions))
         (find (rest transitions) current))]))
     


; fsm fsm-state -> simulationstate
; match the keys pressed with the given fsm
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))
(define (simulate2 a-bw s0)
  (big-bang (make-fs a-bw s0)
            [to-draw render]
            [on-key find-next-state]))

;(simulate fsm-traffic "red")
(simulate2 bw "black")
                                       
