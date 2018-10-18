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


(require test-engine/racket-tests)
; list of numbers -> list of numbers
; converts a list of celcius
; temperature to fahrenheit

(define (cf* l)
  (cond
    [(empty? l) '()]
    [else (cons
           (C2F (first l))
           (cf* (rest l)))]))

; number -> number
; converts one celcius 
; temperature to fahrenheit
(define (C2F c)
  (+ (* 9/5 c) 32))

; inventory -> list of strings
; extracts the names of
; toys from an inventory
(define (names i)
  (cond
    [(empty? i) '()]
    [else (cons
           (IR-name (first i))
           (names (rest i)))]))
(define-struct IR [name price])
; an ir is a structure:
; (make-IR string number)
; an inventory is one of:
; - '()
; - (cons IR inventory)

(define (map1 k g)
  (cond
    [(empty? k) '()]
    [else (cons 
           (g (first k))
           (map1 (rest k) g))]))

; list of numbers -> list of numbers
(define (cf*-from-map1 l) (map1 l C2F))
; inventory -> list of strings
(define (names-from-map1 i) (map1 i IR-name))

(check-expect (cf* (list 100 0 -40))
              '(212 32 -40))
(check-expect (names
               (list (make-IR "doll" 21) (make-IR "bear" 13)))
              '("doll" "bear"))

(check-expect (cf*-from-map1 '(100 0 -40)) '(212 32 -40))
(check-expect (names-from-map1 (list (make-IR "doll" 21) (make-IR "bear" 13))) '("doll" "bear"))

; number -> [list of number]

; tabulates sin bw n and 0 (incl) in a list
(define (tab-sin n)
  (cond
    [(zero? n) (list (sin 0))]
    [else (cons (sin n)
                (tab-sin (sub1 n)))]))

; number-> list of number
; tabulates sqrt bw n and 0 (incl] in a list
(define (tab-sqrt n)
  (cond
    [(zero? n 0) (list (sqrt 0))]
    [else (cons (sqrt n)
                (tab-sqrt (sub1 n)))]))

; [number -> number] number -> list of numbers

(define (tabulate f n)
  (cond
    [(zero? n) (list (f 0))]
    [else (cons (f n)
                (tabulate f (sub1 n)))]))

(define (tab-sqr n) (tabulate sqr n))
(define (tab-tan n) (tabulate tan n))

; [list of number] -> number
; computes the sum of the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l)
             (sum (rest l)))]))

; [list of number] -> number
; computes the product of the numbers on l
(define (product l)
  (cond
   [(empty? l) 1]
   [else (* (first l) 
            (product (rest l)))]))

; f[numbers -> number] init list-of-number > number
(define (fold1 f init lon)
  (cond
    [(empty? l) init]
    [else (f (first l) (fold1 f init (rest lon)))]))








(test)