#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)
(define-struct editor [pre post] #:transparent)
; an editor is a structure:
; (make-editor lo1s lo1s)
; an lo1s is one of:
; - '()
; - (cons 1string lo1s)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))
; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))


(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 

(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))
(define (editor-text s)
  (text (my-implode s) FONT-SIZE FONT-COLOR))

(define (my-implode los)
  (cond
    [(empty? (rest los)) (first los)]
    [else (string-append (first los)
                         (my-implode (rest los)))]))
(check-expect (my-implode (list "p" "o" "s" "t"))
              "post")

(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))
; Lo1s -> Image
; renders a list of 1Strings as a text image 

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

(define (editor-lft ed)
  (make-editor
   (drop-innermost (editor-pre ed))
   (cons (take-innermost (editor-pre ed))
         (editor-post ed))))
(check-expect (editor-lft (make-editor (list "p" "r" "e")
                                       (list "p" "o" "s" "t")))
              (make-editor (list "p" "r")
                           (list "e" "p" "o" "s" "t")))
(define (drop-innermost los)
  (reverse (rest (reverse los))))
(define (take-innermost los)
  (cond
    [(empty? (rest los)) (first los)]
    [else (take-innermost (rest los))]))
(check-expect (take-innermost (list "a" "b" "c")) "c")



(define (editor-rgt ed)
  (make-editor
   (put-innermost
    (editor-pre ed)
    (first (editor-post ed)))
   (rest (editor-post ed))))
(check-expect (editor-rgt (make-editor (list "p" "r" "e")
                                       (list "p" "o" "s" "t")))
              (make-editor (list "p" "r" "e" "p")
                           (list "o" "s" "t")))
(define (put-innermost los s)
  (reverse (cons s (reverse los))))
(check-expect (put-innermost (list "a" "b" "c") "d") (list "a" "b" "c" "d"))


(define (editor-del ed)
  (make-editor
   (drop-innermost (editor-pre ed))
   (editor-post ed)))

(check-expect (editor-del (make-editor (list "p" "r" "e")
                                       (list "p" "o" "s" "t")))
              (make-editor (list "p" "r")
                           (list "p" "o" "s" "t")))
                

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))


(define (create-editor s1 s2)
  (make-editor s1 s2))


; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s (list ""))
     [on-key editor-kh]
     [to-draw editor-render]))


(test)
