#lang racket

(require test-engine/racket-tests)

; lon -> lon
(define (addx x lon)
  (cond
    [(empty? lon) '()]
    [else (cons (+ x (first lon))
                (addx x (rest lon)))]))

(define (add1* lon) (addx 1 lon))
(define (plus5 lon) (addx 5 lon))

(check-expect (add1* '(1 2 34)) '(2 3 35))
(check-expect (plus5 '(2 4 5)) '(7 9 10))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

(define (extreme R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (R (first l) (extreme R (rest l)))
         (first l)
         (extreme R (rest l)))]))

(define (inf l)
  (extreme < l))
(define (sup l)
  (extreme > l))

(define L (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))

(define (inf2 l)
  (extreme min l))
(define (sup2 l)
  (extreme max l))


; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #f]
    [else (if (equal? s (first los))
              (rest los)
              (occurs s (rest los)))]))
  
(define (function=?at f1 f2)
  (and
   (equal? (f1 1.2) (f2 1.2))
   (equal? (f1 3) (f2 3))
   (equal? (f1 -5.775) (f2 -5.775))))



(test)