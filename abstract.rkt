#lang racket

(require 2htdp/image 2htdp/universe test-engine/racket-tests)

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
    [(empty? lon) init]
    [else (f (first lon) (fold1 f init (rest lon)))]))

(check-expect (fold1 + 0 '(1 2 3 4)) 10)
(check-expect (fold1 * 1 '(2 3 4)) 24)

(define-struct posn [x y])

; [list of posn] -> image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))

; posn image -> image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; constants
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))



; sort-n: lon [number number -> boolean] -> lon
; sort-s: los [string string -> boolean] -> los
; sort: [x] list-of-x [x x -> boolean] -> list-of-x
; [x = [ir -> x]] lox [x x -> boolean] -> lox

; map-n: lon [number -> number] -> lon
; map-s: los [string -> string] -> los

; [x y] [list of x] y [x y -> y] -> y
(define (reduce l base combine)
  (cond
    [(empty? l) base]
    [else (combine (first l)
                   (reduce (rest l) base combine))]))
; [number number] [list of number] number [number -> number -> number] -> number
; [list of number] -> number
(define (sum1 lon)
  (reduce lon 0 +))
(define (product1 lon)
  (reduce lon 1 *))
; [posn emt] [list of posn] emt [posn emt -> img] -> img
(define (render lop)
  (reduce lop emt place-dot))

(check-expect (fold1 place-dot emt (list (make-posn 10 10) (make-posn 20 20)))
              (render (list (make-posn 10 10) (make-posn 20 20))))
                     
; [x] n [n -> x] -> [list of x]
; construct a list by applying f to 0 1 ... n-1
; (build-list n f] == (list (f 0] ...(f (- n 1]]]
(define (my-build-list-helper n f)
  (cond
    [(zero? n) '()]
    [else (cons (f (sub1 n))
                (my-build-list-helper (sub1 n) f))]))

(define (my-build-list n f)
  (reverse (my-build-list-helper n f)))

(check-expect (my-build-list 3 add1)
              (build-list 3 add1))

; [x] [x -> boolean] [list of x] -> [list of x]
; produces a list from those items on lx for which p holds
(define (my-filter p lx)
  (cond
    [(empty? lx) '()]
    [else (if (p (first lx))
              (cons (first lx) (my-filter p (rest lx)))
              (my-filter p (rest lx)))]))
(check-expect (my-filter positive? (list -1 2 0 3))
              (filter positive? (list -1 2 0 3)))
; [x] [list of x] [x x -> boolean] -> [list of x]
; produces a version of lx that is sorted according to cmp
(define (my-sort lx cmp)
  (cond
    [(empty? lx) '()]
    [else (my-insert (first lx) (my-sort (rest lx) cmp) cmp)]))
(define (my-insert x lx cmp)
  (cond
    [(empty? lx) (list x)]
    [else (if (cmp x (first lx))
              (cons x (my-insert (first lx) (rest lx) cmp))
              (cons (first lx)
                    (my-insert x (rest lx) cmp)))]))

(check-expect (my-sort (list 4 6 0 1) >)
              (sort (list 6 4 1 0) >))
  
; [x y] [x -> y] [list of x] -> [list of y]
; construct a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ...(f x-n))
(define (my-map f lx)
  (cond
    [(empty? lx) '()]
    [else (cons (f (first lx)) (my-map f (rest lx)))]))
(check-expect (my-map sub1 (list 3 4 5))
              (map sub1 (list 3 4 5)))

; [x] [x->boolean] [list of x] -> boolean
; determines whether p holds for every item on lx
; (andmap p (list x-1 ... x-n) == (and (p x-1) ..(p x-n))
(define (my-andmap p lx)
  (cond
    [(empty? lx) #t]
    [else (and (p (first lx))
               (my-andmap p (rest lx)))]))

(check-expect (my-andmap positive? '())
              (andmap positive? '()))
(check-expect (my-andmap positive? (list 4 3 2))
              (andmap positive? (list 4 3 2)))
(check-expect (my-andmap positive? (list 3 -2 1))
              (andmap positive? (list 3 -2 1)))

; [x] [x->boolean] [listof x] -> boolean
; determines whether p holds for at least one item on lx
; (ormap p (list x-1 ..x-n)) == (or (p x-1) ...(p x-n))
(define (my-ormap p lx)
  (cond
    [(empty? lx) #f]
    [else (or (p (first lx))
              (my-ormap p (rest lx)))]))

(check-expect (my-ormap positive? '())
              (ormap positive? '()))
(check-expect (my-ormap positive? (list 1 -2))
              (ormap positive? (list 1 -2)))




(define-struct address [first-name last-name street])
; an addr is a structure:
; (make-address string string string)
; interpretation associates an address w a person name

; [list of addr] -> string
; creates a string from first names
; sorted in alphabetical order,
; separated and surrounded by blank spaces

(define (listing l)
  (foldr string-append-with-space " "
         (sort (map address-first-name l) string<?)))

; string string ->string
; appends two strings, prefixes with " "
(define (string-append-with-space s t)
  (string-append " " s t))

(define ex0
  (list (make-address "robert" "findler" "south")
        (make-address "matthew" "flatt" "canyon")
        (make-address "shriram" "krishna" "yellow")))
(check-expect (listing ex0) " matthew robert shriram ")


; [list-of addr] ->string
; creates a string of first names
; sorted in alphabetical order
; separated and surrounded by blank spaces

(define (listing2 l)
  (local (; 1. extract names
          (define names (map address-first-name l))
          ; 2. sort the names
          (define sorted (sort names string<?))
          ; 3. append them, add spaces
          ; string string -> string
          ; appends two strings, prefix with " "
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

(check-expect (listing2 ex0) (listing ex0))


; [list of number] [number number -> boolean]
; -> [list of number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [list of number] -> [list of number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon)
                       (isort (rest alon)))]))
          ; number [list of number] -> [list of number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))




; nelon -> number
; determines the smallest number on l
(define (my-inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local
       ((define smallest-in-rest (my-inf (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))
        


; inventory -> inventory
; creates an inventory from an-inv for all
; those items that cost less than a dollar
(define-struct ir [price] #:transparent)
(define inv (list (make-ir 0.4) (make-ir 0.3)
                  (make-ir 3) (make-ir 3.5)))
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define fi (first an-inv))
             (define in (extract2 (rest an-inv))))
       (cond
         [(<= (ir-price fi) 1)
          (cons fi in)]
         [else in]))]))
(check-expect (extract2 inv) (extract1 inv))

; positive -> list of list of number
(define (identityM n)
  (local
    ((define (identity countdown size)
       (cond
         [(zero? countdown) (list (create-row 0 size))]
         [else (cons (create-row countdown size)
                     (identity (sub1 countdown) size))]))
     (define (create-row 1-at n)
       (cond
         [(zero? n) (if (= n 1-at) (list 1) (list 0))]
         [else (cons (if (= n 1-at) 1 0)
                     (create-row 1-at (sub1 n)))])))
    (identity (sub1 n) (sub1 n))))




(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0)
                                  (list 0 1 0)
                                  (list 0 0 1)))





(test)