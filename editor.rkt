#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define W 300)
(define H 300)
(define C (empty-scene W H))

(define char-w (image-width (text "a" 11 "black")))
(define (str-w n)
  (* char-w n))

(define-struct editor [pre post])
; an editor is a structure
; (make-editor string string}
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

; editor -> image
; display the content of editor on screen

(define (render e)
  (overlay/align "left" "center"
                 (beside (text (editor-pre e) 11 "black")
                         (text (editor-post e) 11 "black"))
                 C))

(check-expect (render (make-editor "hello" "world"))
              (overlay/align "left" "center"
                             (text "helloworld" 11 "black")
                             C))
; editor keyevent -> editor
; add single char to screen

(define (edit e ke)
  (define l (string-length ke))
  ;(match-define (make-editor pre post) e)
  (define pre (editor-pre e))
  (define post (editor-post e))
  (cond 
    [(>= (str-w (string-length (string-append pre post)))
          W)
     e]
    [(and (> l 1) (string=? ke "left")) 
     (make-editor (car (move-left pre post))
                  (cdr (move-left pre post)))]
    [(and (> l 1) (string=? ke "right"))
     (make-editor (car (move-right pre post))
                  (cdr (move-right pre post)))]
    [(> l 1) e]
    [(and (= l 1) (string=? ke "\b"))
     (make-editor (back-space pre) post)]
    [(and (= l 1) (or (string=? ke "\t")
                      (string=? ke "\r")))
     e]
    [else (make-editor (string-append pre ke)
                       post)]))
    
    
                                                    
    
(define (drop-last str len)
  (substring str 0 (sub1 len)))
(define (back-space str)
  (define l (string-length str))
  (if (zero? l) ""
      (drop-last str l)))
(define (break-last str len)
  (if (zero? len)
      (cons "" str)
      (cons
       (substring str 0 
                  (sub1 len))
       (substring str (sub1 len) len))))
  
(define (break-first str len)
  (if (zero? len)
      (cons str "")
      (cons
       (substring str 0 1)
       (substring str 1 len))))

(define (move-left s1 s2)
  (define l (string-length s1))
  (match-define (cons s11 s12) (break-last s1 l))
  (cons s11 (string-append s12 s2)))
(define (move-right s1 s2)
  (define l (string-length s2))
  (match-define (cons s21 s22) (break-first s2 l))
  (cons (string-append s1 s21) s22))
                                                     

(define e0 (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") " ") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world"))
(check-expect (edit (make-editor "" "hello") "\b") (make-editor "" "hello"))
(check-expect (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\r") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "" "hello") "left") (make-editor "" "hello"))
(check-expect (edit (make-editor "hello" "") "right") (make-editor "hello" ""))
(check-expect (edit (make-editor "hello" "world") "up") (make-editor "hello" "world"))

; editor -> editor
(define (main e)
  (big-bang e
            [on-key edit]
            [to-draw render]))
