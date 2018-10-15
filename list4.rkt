#lang racket
(require test-engine/racket-tests 2htdp/image 2htdp/universe)

; List-of-numbers -> List-of-numbers 
; rearranges alon in descending order
 
(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))
 ; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))


(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 1 4)) (list 5 1 4))
(check-expect (insert 12 (list 60 20 -5))
              (list 60 20 12 -5))


(define (sorted>? lot)
  (cond
    [(empty? (rest lot)) #t]
    [else
     (if (< (first lot) (first (rest lot))) #f
         (sorted>? (rest lot)))]))

(check-expect (sorted>? (list 3 2 1)) #t)
(check-expect (sorted>? (list 3 4 1)) #f)

(check-satisfied (sort> (list 1 4 5)) sorted>?)


;;;
(define-struct gp [name score] #:transparent)
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points
(define (sort-score log)
  (cond
    [(empty? log) '()]
    [else (insert-player (first log) (sort-score (rest log)))]))
(define (insert-player player log)
  (cond
    [(empty? log) (cons player '())]
    [else (if (>= (gp-score player) (gp-score (first log)))
              (cons player log)
              (cons (first log) (insert-player player (rest log))))]))

(check-expect (sort-score (list (make-gp "chi" 20)
                                (make-gp "angela" 30)
                                (make-gp "bob" 45)))
              (list (make-gp "bob" 45)
                    (make-gp "angela" 30)
                    (make-gp "chi" 20)))
(check-expect (insert-player (make-gp "chi" 33)
                             (list (make-gp "angela" 44)
                                   (make-gp "bob" 5)))
              (list (make-gp "angela" 44)
                    (make-gp "chi" 33)
                    (make-gp "bob" 5)))
              
;;;
(define-struct email [from date message] #:transparent)
; an email message is a structure
; (make-email string number string)
; interpretation (make-email f d m) reprensents
; text m, sent by f, d seconds after the beginning of time

(define (sort-email loe)
  (cond
    [(empty? loe) '()]
    [else (insert-email (first loe) (rest loe))]))
(define (insert-email em loe)
  (cond
    [(empty? loe) (cons em '())]
    [else (if (>= (email-date em) (email-date (first loe)))
              (cons em loe)
              (cons (first loe) (insert-email em (rest loe))))]))

(define (sort-email2 loe)
  (cond
    [(empty? loe) '()]
    [else (insert-email2 (first loe) (rest loe))]))
(define (insert-email2 em loe)
  (cond
    [(empty? loe) (cons em '())]
    [else (if (string<? (email-from em) (email-from (first loe)))
              (cons em loe)
              (cons (first loe) (insert-email2 em (rest loe))))]))

; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))


(define (search-sorted n lon)
  (cond
    [(empty? lon) #f]
    [(> n (first lon)) #f]
    [else (or (= n (first lon))
              (search-sorted n (rest lon)))]))

(check-expect (search-sorted 5 (list 4 3 2 1)) #f)
(check-expect (search-sorted 3 (list 9 8 7)) #f)
(check-expect (search-sorted 3 (list 5 4 3 2)) #t)
          
;;;
(define (prefixes los)
  (cond
    [(empty? los) '()]
    [else (cons los
                (prefixes (drop-last los)))]))
(check-expect (prefixes (list 1 2 3 4))
              (list (list 1 2 3 4)
                    (list 1 2 3)
                    (list 1 2)
                    (list 1)))
(define (drop-last los)
  (cond
    [(empty? (rest los)) '()]
    [else (cons (first los) (drop-last (rest los)))]))
(check-expect (drop-last (list 1 2 3 4 5)) (list 1 2 3 4))

(define (suffixes los)
  (cond
    [(empty? los) '()]
    [else (cons los (suffixes (rest los)))]))
(check-expect (suffixes (list 1 2 3 4))
              (list (list 1 2 3 4)
                    (list 2 3 4)
                    (list 3 4)
                    (list 4)))


;;;
(define-struct posn [x y] #:transparent)
(define tri
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))
(define squ
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)


; a Polygon is one of: 
; – (cons Posn (cons Posn (cons Posn '()))) 
; – (cons Posn Polygon)

; a plain background image 
(define MT (empty-scene 50 50))

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)
; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line
       (connect-dots img (rest p))
       (first p)
       (second p))]))

(check-expect (connect-dots MT tri)
              (scene+line
               (scene+line MT 20 10 20 20 "red")
               20 20 30 20 "red"))

; Image Polygon -> Image 
; adds an image of p to img
(define (render-polygon img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

; Polygon -> Posn
; extracts the last item from p

; NELoP -> Posn
; extracts the last item from p
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q)
    "red"))


(define (render-poly2 lop)
  (connect-dots MT (cons (last lop) lop)))


(test)