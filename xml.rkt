#lang racket
(require 2htdp/batch-io
  2htdp/universe 
  2htdp/image test-engine/racket-tests)

(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; xexper.v2 -> [list of attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else 
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; [list of attribute] or xexpr.v2 -> boolean
; is x a list of attributes

(define (list-of-attributes? x)
  (cond
    [(empty? x) #t]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

(define (list-of-attributes?2 x)
  (or (empty? x) (cons? (first x))))

(check-expect (list-of-attributes?2 '((action))) (list-of-attributes?2 '((action))))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; xexper.v2 -> name
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

(check-expect (xexpr-name e0) 'machine)
   

(define (find-attr sym loa)
  (assq sym loa))

(check-expect (find-attr 'initial '((initial "X"))) '(initial "X"))
(check-expect (find-attr 'a '((initial "X"))) #f)

; xexper.v2 -> [list of content]
; retrieves the content of xe
(define (xexpr-content xe)
  (filter (lambda (a) (not (list-of-attributes? a))) (rest xe)))

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

;; rendering xml enumerations

; an xword is '(word ((text string]]]

(define w0 '(word ((text "hello"))))
(define w1 '(word ((text "world"))))
(define w2 '(word ((text "there"))))
(define (good-struct? s)
  (or (empty? s)
      (and 
       [list? s]
       [= 2 (length s)] 
       [equal? 'text (first s)] 
       [string? (second s)])))
(define (word? w)
  (and [equal? 'word (first w)]
       [list? (second w)]
       [my-and (map good-struct? (second w))]))

(define (my-and lst)
  (cond
    [(empty? lst) #t]
    [else (and (first lst) (my-and (rest lst)))]))

(check-expect (word? w0) #t)
(check-expect (word? '(word (text "hi"))) #f)
(check-expect (word? '(word (()))) #t)
(check-expect (word? '(word ((text "h" "i")))) #f)

(define (word-text w)
  (last (first (second w))))

(check-expect (word-text w0) "hello")

; an xenum.v1 is one of:
; - (cons 'ul [list of xitem.v1])
; - (cons 'ul (cons attributes [list of xitem.v1]))
; an xitem.v1 is one of:
; - (cons 'li (cons xword '()))
; - (cons 'li (cons attributes (cons xword '())))

(define enum0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define enum1
  '(ul
    (li (word ((text "one"))))
    (li (ul (li (word ((text "one"))))
            (li (word ((text "two"))))))))

(define BT1 (circle 2 "solid" "black"))
(define enum0-rendered
  (above/align
   'left 
   (beside/align 'center BT1 (text "one" 12 'black))
   (beside/align 'center BT1 (text "two" 12 'black))))


(define (render-item1 i)
  (local (
          (define element (second i))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT1 item)))

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; xitem.v1 image -> image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

; an xitem.v2 is one of:
; - (cons 'li (cons xword '()))
; - (cons 'li (cons [list of attribute] (list xword)))
; - (cons 'li (cons xenum.v2 '(}}}
; - (cons 'li (cons [list of attribute] (list xenum.v2}}}

; an xenum.v2 is one of:
; - (cons 'ul [list of xitem.v2])
; - (cons 'ul (cons [list of attribute] [list of xitem.v2]))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))

; image -> image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BT item))

; xenum.v2 -> image
; renders and xenum.2 as an image
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; xitem.v2 image -> image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; xitem.v2 -> image
; renders one xitem.v2 as an image
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))
    
;; str xenum -> number
(define (count-words str xenum)
  (local ((define content (map first (map xexpr-content (xexpr-content xenum)))))
    (foldr (lambda (nxt init)
             (count-enum-h str nxt init)) 0 content)))
    
   
(define (count-enum-h str w-o-enum counter)
  (cond
    [(word? w-o-enum) 
     (if (equal? str (word-text w-o-enum)) 
         (add1 counter)
         counter)]
    [else
     (+ counter
        (count-words str w-o-enum))]))
          

;; str item -> number

(check-expect (count-words "hello" enum0) 0)
(check-expect (count-words "one" enum0) 1)
(check-expect (count-words "one" enum1) 2)

; 
(define (replace str str2 xenum)
  (local ((define head (first xenum))
          (define neck (xexpr-attr xenum))
          (define content (xexpr-content xenum)))
    (list head neck
          (foldr (lambda (nxt init)
                   (cons (replace-h str str2 nxt) init)) '() content))))

(define (replace-h str str2 w-o-enum)
  (cond
    [(word? w-o-enum)
     (if (equal? str (word-text w-o-enum))
         `(word ((text ,str2)))
         w-o-enum)]
    [else
     (replace str str2 w-o-enum)]))

(check-expect (replace "one" "three" enum0)
'(ul () ((li () ((word ((text "three"))))) (li () ((word ((text "two"))))))))

(check-expect (replace "one" "three" enum1)
              '(ul
                ()
                ((li () ((word ((text "three")))))
                 (li
                  ()
                  ((ul
                    ()
                    ((li () ((word ((text "three"))))) 
                     (li () ((word ((text "two"))))))))))))


; an fsm is a [list of 1transition]
; a 1transition is a list of two items:
; (cons fsm-state (cons fsm-state '()))
; an fsm-state is a string that specifies a color

; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; fsm fsm-state -> fsm-state
; matches the keys pressed by a player with the given fsm
(define (simulate state0 transitions)
  (big-bang state0 ; fsm-state
            [to-draw
             (lambda (current)
               (square 100 "solid" current))]
            [on-key 
             (lambda (current key-event)
               (find transitions current))]))

; [X Y] [list of [list of X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))


(check-expect (find '((1 2) (3 3) (4 1)) 4) 1)

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

; an xmachine is a nested list of this shape:
; `(machine ((initial ,fsm-state)) [List of X1T])
; an x1t is a nested list of this shape:
; `(action ((state ,fsm-state) (next ,fsm-state)))

(define bw
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))


; xmachine -> fsm-state
; simulates an fsm via the given configuration

(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; xmachine -> fsm-state
; extracts and translates the transition table from xm0

(define (find-attr2 sym loa)
  (local ((define res (assq sym loa)))
    (if (cons? res) (second res) #f)))

(define (xm-state0 xm0)
  (find-attr2 'initial (xexpr-attr xm0)))

; xmachine -> [list of 1 transition]
; extracts the transition table from xm

(define (xm->transitions xm)
  (local (; x1t -> 1transition
          (define (xaction->action xa)
            (list (find-attr2 'state (xexpr-attr xa))
                  (find-attr2 'next (xexpr-attr xa)))))
    (map xaction->action (xexpr-content xm))))



(check-expect (xm-state0 xm0) "red")
(check-expect (xm->transitions xm0) fsm-traffic)


; an xexpr.v3 is one of
; -sym
; -string
; -number
; - (cons symbol (cons attribute*.v3 [list of xexpr.v3]))
; - (cons symbol [list of xexpr.v3])
; an attribute*.v3 is a [list of attribute.v3]
;
; an attribute.v3 is a list of 2 items
; (list symbol string)

; any -> boolean
; is x an xexpr.v3
; effect displays bad piece if x is not an xexpr.v3
(define (xexpr? x) ...)

; string -> xexpr.v3
; produces the first xml element in file f
(define (read-xexpr f) ...)

; string -> boolean
; #f, if this url returns a 404
; #t otherwise
(define (url-exists? u) ...)

; string -> [maybe xexpr.v3]
; retrieves the firts xml (html) element from url u
; #f if (not (url-exists? u))
(define (read-plain-xexpr/web u) ...)

; string -> [maybe xexpr.v3]
; retrieves the first xml (html) element from url u
; #f if (not (url-exists? u))
(define (read-xexpr/web u) ...)







(test)