#lang racket
(require test-engine/racket-tests)

; str los -> boolean
(define (contain? str los)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (or
      (string=? str (first los))
      (contain? str (rest los)))]))

(check-expect (contain? "a" (cons "b" '())) #f)
(check-expect (contain? "a" (cons "a" '())) #t)


; los -> number
(define (how-many? los)
  (cond
    [(empty? los) 0]
    [else (+ 1 (how-many? (rest los)))]))
(check-expect (how-many? '()) 0)
(check-expect (how-many? (cons "a" '())) 1)

; loa -> number
; a list of amount is one of:
; - '()
; - (cons positivenumber listofamounts)

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else
     (+ (first loa) (sum (rest loa)))]))

(check-expect (sum (cons 10 '())) 10)
(check-expect (sum (cons 10 (cons 20 '()))) 30)

; lon -> boolean
(define (pos? lon)
  (cond
    [(empty? lon) #t]
    [else
     (and
      (> (first lon) 0)
      (pos? (rest lon)))]))
(check-expect (pos? (cons -1 '())) #f)

(define (checked-sum loa)
  (if (pos? loa)
      (sum loa)
      #f))

(check-expect (checked-sum (cons -1 '())) #f)
(check-expect (checked-sum (cons 10 '())) 10)


; lob -> boolean
(define (all-true lob)
  (cond
    [(empty? lob) #t]
    [else
     (if (false? (first lob)) #f
         (all-true (rest lob)))]))

(check-expect (all-true (cons #t '())) #t)
(check-expect (all-true (cons #t (cons #f '()))) #f)

(define (one-true lob)
  (cond
    [(empty? lob) #f]
    [else
     (if (not (false? (first lob))) #t
         (one-true (rest lob)))]))
(check-expect (one-true (cons #f '())) #f)
(check-expect (one-true (cons #f (cons #f (cons #t '())))) #t)


; los -> str
; concat
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(define (cat los)
  (cond
    [(empty? los) ""]
    [else
     (string-append
      (first los)
      (cat (rest los)))]))

; imageorfalse is one of:
; -image
; -#f

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #f]
    [else
     (if (not (= (first loi) n)) #t 
         (ill-sized? (rest loi) n))]))

(check-expect (ill-sized? (list 8 8 8 1) 8) #t)
(check-expect (ill-sized? (list 8 8 8) 8) #f)

; average
; non empty list of temp is one of:
; - (cons CT '())
; - (cons CT NELot)
;

(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)
(define (average ne-l)
  (/ (sum1 ne-l)
     (how-many1 ne-l)))
(define (sum1 ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else
     (+ (first ne-l) (sum1 (rest ne-l)))]))
(define (how-many1 ne-l)
  (cond
    [(empty? (rest ne-l)) 1]
    [else
     (+ 1 (how-many1 (rest ne-l)))]))

(define (sorted>? lot)
  (cond
    [(empty? (rest lot)) #t]
    [else
     (if (< (first lot) (first (rest lot))) #f
         (sorted>? (rest lot)))]))

(check-expect (sorted>? (list 3 2 1)) #t)
(check-expect (sorted>? (list 3 4 1)) #f)

(define (all-true1 lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
     (if (false? (first lob)) #f
         (all-true1 (rest lob)))]))
(define (one-true1 lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
     (if (not (false? (first lob))) #t
         (one-true1 (rest lob)))]))

(check-expect (all-true1 (list #t #t #f)) #f)
(check-expect (all-true1 (list #t #t #t)) #t)
(check-expect (one-true1 (list #f #f #t)) #t)
(check-expect (one-true1 (list #f #f #f)) #f)

; an N is one of:
; 0
; (add1 N)
; interpretation: represents the counting numbers

; n string -> los
; creates a list of n copies of s

(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n)
     (cons s (copier (sub1 n) s))]))




(test)
