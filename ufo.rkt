#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

; a worldstate is a number
; interpretation: number of pixels between the top and the ufo

(define W 300)
(define H 100)
(define CLOSE (/ H 3))
(define C (empty-scene W H))
(define UFO (overlay
             (circle 10 "solid" "green")
             (rectangle 30 10 "solid" "green")))

; worldstate -> worldstate
(define (main y0)
  (big-bang y0
            [on-tick nxt]
            [to-draw render/status]))

; worldstate -> worldstate
; computes next location of ufo
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))

; worldstate -> image
; places ufo at given height into the center of C
(check-expect(render 11) (place-image UFO 150 11 C))
(define (render y)
  (place-image UFO 150 y C))
; add a status line to the scene created by render
(check-expect (render/status 10)
              (place-image (text "descending" 11 "green")
                           10 10
                           (render 10)))
(define (render/status y)
  (place-image 
   (cond
     [(<= 0 y CLOSE)
      (text "descending" 11 "green")]
     [(<= CLOSE y H)
      (text "closing in" 11 "orange")]
     [else
      (text "landed" 11 "red")])
     20 20 (render y)))