#lang racket
(require 2htdp/batch-io
  2htdp/image 2htdp/universe test-engine/racket-tests)


(define (append-from-fold l1 l2)
  (local
    ((define (add-last x l)
       (cond [(empty? l) (list x)]
             [else (cons (first l) (add-last x (rest l)))])))
    (foldl add-last l1 l2)))

(define (append-from-foldr l1 l2)
  (local
    ((define (add-last x l)
       (cond [(empty? l) (list x)]
             [else (cons (first l) (add-last x (rest l)))])))
    (foldr add-last l1 (reverse l2))))
(check-expect (append-from-fold (list 1 2 3) (list 4 5))
              (append (list 1 2 3) (list 4 5)))
 (check-expect (append-from-foldr (list 1 2 3) (list 4 5))
               (append-from-fold (list 1 2 3) (list 4 5)))

(define (find-name n lon)
  (local
    ((define (include? x) (include-in? n x)))
    (ormap include? lon)))
 
(check-expect (find-name "a" (list "b" "a")) #t)
(check-expect (find-name "a" (list "b" "bc")) #f)

(define (find-names n lon)
  (local
    ((define (include? x) (include-in? n x)))
    (andmap include? lon)))

(check-expect (find-names "a" (list "ab" "ac")) #t)

(define (check-length l lon)
  (local
    ((define (pass? x) (<= (length x) l)))
    (andmap pass? lon)))

(check-expect (check-length 3 (list "a" "bc" "b")) #t)

(define (include-in? n n2)
  (define l (string->list n))
  (define l2 (string->list n2))
  (include?-helper l l2))

(check-expect (include-in? "a" "ab") #t)
(check-expect (include-in? "a" "bc") #f)

(define (include?-helper l l2)
  (cond
    [(empty? l) #t]
    [else (and (char=? (first l) (first l2))
               (include?-helper (rest l) (rest l2)))]))
 
(define (sum l) (foldl + (first l) (rest l)))
(define (product l) (foldl * (first l) (rest l)))

(check-expect (sum (list 2 3 4)) (+ 2 3 4))
(check-expect (product (list 2 3 4)) (* 2 3 4))

(define (compose loi)
  (foldl beside empty-image loi))

(check-expect (compose (list (circle 10 "solid" "red") (circle 10 "solid" "blue")))
              (beside (circle 10 "solid" "blue") (circle 10 "solid" "red")))

(define (map-from-fold f ls)
  (local ((define (collect nxt init) (cons (f nxt) init)))
    (foldr collect '() ls)))

(check-expect(map-from-fold add1 (list 1 2 3)) (list 2 3 4))

(define (prefixes ls)
  (local
    ((define (f nxt init)
       (append init (list (drop-right (last init) 1)))))
    (foldl f (list ls) (drop ls 1))))

(check-expect (prefixes (list 1 2 3))
              (list (list 1 2 3) (list 1 2) (list 1)))

(define (suffixes ls)
  (local
    ((define (f nxt init)
       (append init (list (drop (last init) 1)))))
    (foldl f (list ls) (drop-right ls 1))))

(check-expect (suffixes (list 1 2 3))
              (list (list 1 2 3) (list 2 3) (list 3)))

(define DICT (read-lines "words.txt"))
(define DICT2 (map string->list DICT))
(define ALPHABET (string->list "abcdefghijklmnopqrstuvwxyz"))


(define (how-many c lst)
  (local
    ((define (f nxt init)
       (if (char=? c (first nxt)) (add1 init) init)))
    (foldl f 0 lst)))
(define-struct posn [x y] #:transparent)
(define (how-manys lst)
  (local
    ((define (f nxt init)
       (cons (make-posn nxt (how-many nxt DICT2)) init)))
    (foldl f '() lst)))
; how-manys ALPHABET

(define (posn< p1 p2)
  (< (posn-y p1) (posn-y p2)))

(define (sort-posn lop)
  (cond
    [(empty? (rest lop)) lop]
    [else (insert-posn (first lop) (sort-posn (rest lop)))]))

(define (insert-posn p lop)
  (cond
    [(empty? lop) (list p)]
    [else (if (posn< p (first lop))
              (cons (first lop) (insert-posn p (rest lop)))
              (cons p (insert-posn (first lop) (rest lop))))]))

(define (most-frequent)
  (define l (how-manys ALPHABET))
  (define l2 (sort-posn l))
  (first l2))
(check-expect (most-frequent) (make-posn #\s 43939))

(define (collect c lst)
  (local
    (
     (define (f nxt init)
       (if (char=? c (first nxt)) (cons nxt init) init)))
    (foldl f '() lst)))

(define (collects lst)
  (local
    ((define (f nxt init)
       (cons (collect nxt DICT2) init)))
    (foldr f '() lst)))

(define-struct ir [price])
; [list of IR] number -> boolean
(define (find l th)
  (local
    (; IR -> boolean
     (define (acceptable? ir)
       (<= (ir-price ir) th)))
  (filter acceptable? l)))

; [list of ir] number -> boolean
(define (my-find l th)
  (filter (lambda (ir) (<= (ir-price ir) th)) l))


(lambda (n) (< n 10))
(lambda (a b) (number->string (* a b)))
(lambda (n) (if (even? n) 0 1))
(lambda (ir1 ir2) (= (ir-price ir1) (ir-price ir2)))
(lambda (p im) (place-image (circle 10 "solid" "red") (posn-x p) (posn-y p) im))

(define (dots lop)
  (define DOT (circle 10 "solid" "red"))
  (define B (empty-scene 100 100))
  (foldr (lambda (p im)
           (place-image DOT (posn-x p) (posn-y p) im))
         B lop))

; list of posn -> list of posn
(define (add-3-to-all lop)
  (map (lambda (p) (make-posn (+ (posn-x p) 3) (posn-y p))) lop))

; list of posn -> list of posn
(define (keep-good lop)
  (filter (lambda (p) (<= (posn-y p) 100)) lop))
                       
; list of posn -> boolean
(define (close? lop pt)
  (define CLOSENESS 5)
  (define (close-to p1 p2 cl)
    (and (< (abs (- (posn-x p1) (posn-x p2))) cl)
         (< (abs (- (posn-y p1) (posn-y p2))) cl)))
  (ormap (lambda (p) (close-to p pt CLOSENESS)) lop))





(test)