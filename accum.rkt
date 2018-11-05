#lang racket
(require test-engine/racket-tests)

; [list of number] -> [list of number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))

; number [list of number] -> [list of number]
; adds n to each number on l

(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

(define (add-to-each n l)
  (map (lambda (x) (+ n x)) l))
  
;;;;

(define a-sg 
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

; a simplegraph is a [list of connection]
; a connection is a list of 2 items
; (list node node)
; a node is a symbol

; node node simplegraph -> boolean
; is there a path from origin to destination
; in the simple graph sg

(check-expect (path-exists? 'A 'E a-sg) #t)
(check-expect (path-exists? 'A 'F a-sg) #f)

(define (path-exists? origin destination sg)
  (local (; node node simplegraph [list of node] -> boolean
          (define (path-exists?/a origin seen)
            (cond
              [(symbol=? origin destination) #t]
              [(member? origin seen) #f]
              [else (path-exists?/a (neighbour origin sg)
                                    (cons origin seen))])))
    (path-exists?/a origin '())))

; node simplegraph -> node
; determine the node that is connected to a-node in sg
(check-expect (neighbour 'A a-sg) 'B)
(check-error (neighbour 'G a-sg) "neighbour: not a node")
(define (neighbour a-node sg)
  (cond
    [(empty? sg) (error "neighbour: not a node")]
    [else (if (symbol=? (first (first sg)) a-node)
              (second (first sg))
              (neighbour a-node (rest sg)))]))


(define (member? x lst)
  (ormap (lambda (e) (equal? x e)) lst))

; [list of x] -> [list of x]
; constructs the reverse of alox

(check-expect (invert '(a b c)) '(c b a))

(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))

; x [list of x] -> [list of x]
; adds an-x to the end of alox

(check-expect (add-as-last 'a '(c b)) '(c b a))

(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last an-x (rest alox)))]))


(define (invert.v2 alox0)
  (local (; [list of x] [list of x] -> [list of x]
          ; constructs the reverse of alox
          ; accumulator is the list of all those
          ; items on alox0 that precede alox
          ; in reverse order
          (define (invert/a alox a)
            (cond
              [(empty? alox) a]
              [else
               (invert/a (rest alox)
                         (cons (first alox) a))])))
    (invert/a alox0 '())))

(define (sum.v2 alon0)
  (local (; [list of number] ??? -> number
          ; computes the sum of the numbers on alon
          ; accumulator a is the sum of the numbers
          ; that alon lacks from alon0
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                           (+ (first alon) a))])))
    (sum/a alon0 0)))

; n -> n
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)

(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n0)
  (local (; N N -> N
          ; computes (* n (- n 1] (- n 2] ..1]
          ; accumumlator a is the product of the
          ; natural numbers in the interval [n0,n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))

(define-struct node [left right])
; a tree is one of:
; '()
; (make-node tree tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

(check-expect (height example) 3)

; a lam is one of:
; a symbol
; (list 'lambda (list symbol) lam)
; (list lam lam)

(define e0 'x)
(define e1 '(lambda (x) x))
(define e2 '(lambda (x) y))
(define e3 '(lambda (y) (lambda (x) y)))
(define e4 '((lambda (x) (x x)) (lambda (x) (x x))))
(define e5 '(((lambda (y) (lambda (x) y)) (lambda (z) z))
             (lambda (w) w)))

(define (is-var? lam)
  (symbol? lam))
(define (is-lambda? lam)
  (and
   (list? lam)
   (equal? 'lambda (first lam))))

(define (is-app? lam)
  (and
   ;(not (is-var? lam)) (not (is-lambda? lam))))
   (list? lam)
   (= 2 (length lam))))

(define (lambda-para lam)
  (first (second lam)))
(define (lambda-body lam)
  (last lam))

(define (app-fun lam)
  (first lam))
(define (app-arg lam)
  (last lam))

(define (declareds lam)
  (cond
    [(is-var? lam) '()]
    [(is-lambda? lam)
     (cons (lambda-para lam)
           (declareds (lambda-body lam)))]
    [(is-app? lam)
     (append (declareds (app-fun lam))
             (declareds (app-arg lam)))]))
             
; lam -> lam
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a lambda
; expression whose parameter is s

(check-expect (undeclareds e1) e1)
(check-expect (undeclareds e2) '(lambda (x) *undeclared))
(check-expect (undeclareds e3) e3)
(check-expect (undeclareds e4) e4)

(define (undeclareds le0)
  (local
    (; lam [list of declareds] -> lam
     ; accumumlator a rerpesents the declareds
     ; on the path from le0 to le
     (define (undeclareds/a le declareds)
       (cond
         [(is-var? le) (if (member? le declareds) le '*undeclared)]
         [(is-lambda? le)
          (local 
            ((define para (lambda-para le))
             (define body (lambda-body le))
             (define newd (cons para declareds)))
            (list 'lambda (list para)
                  (undeclareds/a body newd)))]
         [(is-app? le)
          (local ((define fun (app-fun le))
                  (define arg (app-arg le)))
            (list
             (undeclareds/a fun declareds)
             (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))


(define (undeclareds2 le0)
  (local
    (; lam [list of declareds] -> lam
     ; accumumlator a rerpesents the declareds
     ; on the path from le0 to le
     (define (undeclareds/a le declareds)
       (cond
         [(is-var? le) (if (member? le declareds) 
                           (list '*declared le)
                           (list '*undeclared le))]
         [(is-lambda? le)
          (local 
            ((define para (lambda-para le))
             (define body (lambda-body le))
             (define newd (cons para declareds)))
            (list 'lambda (list para)
                  (undeclareds/a body newd)))]
         [(is-app? le)
          (local ((define fun (app-fun le))
                  (define arg (app-arg le)))
            (list
             (undeclareds/a fun declareds)
             (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

(define e6 '((lambda (x) y) (lambda (y) y)))
(define e7 '(lambda (*undeclared) ((lambda (x) (x *undeclared)) y)))
                         


                    




         


 
(test)