#lang racket
(require test-engine/racket-tests)

; data definition:
; bsl expression's representation is one of:
; number -> number
; [number number] -> number

; example:
; bsl exp       representation     value
; 3             3                  3
; (+ 1 1}       (make-add 1 1)     2

(define-struct add [left right] #:transparent)
(define-struct mul [left right] #:transparent)

(define (eval-exp exp)
  (cond
    [(number? exp) exp]
    [(add? exp) (+ (eval-exp (add-left exp))
                   (eval-exp (add-right exp)))]
    [(mul? exp) (* (eval-exp (mul-left exp))
                   (eval-exp (mul-right exp)))]))
           
(check-expect (eval-exp 3) 3)
(check-expect (eval-exp (make-add 1 1)) 2)
(check-expect (eval-exp (make-mul 3 10)) 30)
(check-expect (eval-exp (make-add (make-mul 1 1) 10)) 11)

(define-struct band [left right])
(define-struct bor [left right])
(define-struct bnot [center])

; #t #f
; boolean -> boolean
; [boolean boolean] -> boolean
(define (true? x) (not (false? x)))
(define (eval-bool-expression exp)
  (cond
    [(boolean? exp) exp]
    [(band? exp) (and (eval-bool-expression (band-left exp))
                      (eval-bool-expression (band-right exp)))]
    [(bor? exp) (or (eval-bool-expression (bor-left exp))
                    (eval-bool-expression (bor-right exp)))]
    [(bnot? exp) (not (eval-bool-expression (bnot-center exp)))]))
                    
(check-expect (eval-bool-expression #t) #t)
(check-expect (eval-bool-expression (make-band #t #f)) #f)
(check-expect (eval-bool-expression (make-bor #t #f)) #t)
(check-expect (eval-bool-expression (make-bnot #t)) #f)

(define (atom? x)
  (or (number? x) (string? x) (symbol? x)))
(define WRONG "wrong input")
; s-expr -> bsl-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; sl -> bsl-expr
(define (parse-sl s)
  (local ((define l (length s)))
    (cond
      [(< l 3) (error WRONG)]
      [(and (= l 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s))
                    (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s))
                    (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; atom -> bsl-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 2 3)) (make-mul 2 3))
(check-expect (parse 2) 2)
(check-error (parse '(+ 1 2 3)))

(define (interpreter-expr s)
  (eval-exp (parse s)))

(check-expect (interpreter-expr '(+ 1 2)) 3)
(check-expect (interpreter-expr '(+ 1 (* 2 3))) 7)
(check-expect (interpreter-expr 2) 2)

; a bsl-var-expr is one of:
; - number
; - symbol
; - (make-add bsl-var-expr bsl-var-expr)
; - (make-mul bsl-var-expr bsl-var-expr)

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))

(check-expect (subst 'x 'x 3) 3)
(check-expect (subst (make-add 'x 3) 'x 3) (make-add 3 3))
(check-expect (subst (make-add (make-mul 'x 'x) 3) 'x 3)
              (make-add (make-mul 3 3) 3))

(define (numeric? vex)
  (cond
    [(symbol? vex) #f]
    [(number? vex) #t]
    [(add? vex)
     (and (numeric? (add-left vex)) (numeric? (add-right vex)))]
    [(mul? vex)
     (and (numeric? (mul-left vex)) (numeric? (mul-right vex)))]))

(check-expect (numeric? 'x) #f)
(check-expect (numeric? 3) #t)
(check-expect (numeric? (make-add 1 (make-mul 2 2))) #t)
(check-expect (numeric? (make-add 2 (make-mul 'x 3))) #f)

(define (eval-variable vex)
  (if (numeric? vex)
      (eval-exp vex)
      (error WRONG)))

(check-expect (eval-variable 3) 3)
(check-error (eval-variable 'x))
(check-expect (eval-variable (make-add 1 (make-mul 2 2))) 5)
(check-error (eval-variable (make-add 1 (make-mul 2 'x))))

; an al (association list) is [list of association]
; an association is a list of two items
; (cons symbol (cons number '()))

(define (eval-variable* vex da)
  (define new-vex
    (foldr (lambda (nxt init)
             (subst init (first nxt) (last nxt)))
           vex da))
  (eval-variable new-vex))
(check-expect (eval-variable* 3 '((x 3))) 3)
(check-expect (eval-variable* 'x '((x 3))) 3)
(check-expect (eval-variable* (make-add 1 (make-mul 'x 'y))
                              '((x 2) (y 3)))
              7)
(check-expect (eval-variable* (make-add 1 1)
                              '((z 3))) 2)
    
; bsl-var-expr al -> number
(define (eval-var-lookup vex da)
  (cond
    [(number? vex) vex]
    [(symbol? vex) (second (assq vex da))]
    [(add? vex) (eval-exp
                 (make-add (eval-var-lookup
                            (add-left vex) da)
                           (eval-var-lookup
                            (add-right vex) da)))]
    [(mul? vex) (eval-exp
                 (make-mul (eval-var-lookup
                            (mul-left vex) da)
                           (eval-var-lookup
                            (mul-right vex) da)))]))
(check-expect (eval-var-lookup 3 '((x 2))) 3)
(check-expect (eval-var-lookup 'x '((x 2))) 2)
(check-expect (eval-var-lookup
               (make-add 1 (make-mul 'x 'y))
               '((x 1) (y 2))) 3)
              
; bsl function expr
; name exp
; fex sym sym fex -> value (number)
(define (eval-definition1 ex f x b)
  (eval-exp (subst b x (eval-exp ex)))) 
  
(check-expect (eval-definition1 2 'f 'x (make-add 2 'x))
              4)

;  (local
;    ((define value (eval-definition1 arg f x b))
;     (define plugd (subst b x arg-value)))
;   (eval-definition1 plugd f x b)))

; fex
; - number
; - symbol
; - (make-add vex vex]
; - (make-mul vex vex]
; - (list symbol vex)


; bsl-fun-def
; symbol symbol body(vex]
; 'f 'x (make-add vex vex)

; 'g 'y ('f (make-mul 2 y]
; 'h 'v (make-add ('f 'v] ['g 'v]]

; bsl-fun-def* symbol -> bsl-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) '(g y (f (make-mul 2 'y))))
(define (lookup-def da f)
  (assq f da))
(define da-fgh
  (list
   (list 'f 'x (make-add 3 'x))
   (list 'g 'y '(f (make-mul 2 'y)))
   (list 'h 'v (make-add '(f v) '(g v)))))

(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(list? ex)  
     (local
       ((define res (lookup-def da-fgh (first ex)))
        (define var (second res))
        (define body (last res)))
       (eval-definition1 (second ex) (first ex) var body))]
    [(add? ex) (+ (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]))
          
    
(define e (make-add '(f 1) '(g 3)))
(check-expect (eval-function* 
               (make-add '(f 1) '(g 3)) da-fgh)
              (eval-exp (make-add (make-add 3 1) 
                                  (make-add 3 (make-mul 2 3)))))
      






(test)