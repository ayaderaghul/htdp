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
                         (subst (mul-right ex) x v))]
    [(list? ex) (map (lambda (el)
                       (subst el x v)) ex)]))

(check-expect (subst 'x 'x 3) 3)
(check-expect (subst (make-add 'x 3) 'x 3) (make-add 3 3))
(check-expect (subst (make-add (make-mul 'x 'x) 3) 'x 3)
              (make-add (make-mul 3 3) 3))
(check-expect (subst (make-add 3 3) 'x 2) (make-add 3 3))

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
  (cond
    [(number? ex) ex]
    [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                  (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                  (eval-definition1 (mul-right ex) f x b))]
    [(list? ex) (eval-definition1
                 (subst b x
                        (eval-definition1
                         (second ex) f x b))
                 f x b)]))
                  
   
  
;  (eval-exp (subst b x (eval-exp ex)))) 
  
(check-expect (eval-definition1 2 'f 'x (make-add 2 'x))
              2)
(check-expect (eval-definition1 `(f ,(make-mul 1 3))
                                'f 'x (make-add 2 'x))
              5)
(check-expect (eval-definition1
               (make-add 1 `(f ,(make-mul 1 3)))
               'f 'x (make-add 2 'x))
              6)
(check-expect (eval-definition1
               (make-add `(f ,(make-mul 1 2))
                         `(f ,(make-add 1 1)))
               'f 'x (make-add 2 'x))
              8)

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
(check-expect (lookup-def da-fgh 'g) `(g y (f ,(make-mul 2 'y))))
(define (lookup-def da f)
  (local ((define res (assq f da)))
          (if res res (error "not found"))))
(check-error (lookup-def da-fgh 'q))
(define da-fgh
  (list
   (list 'f 'x (make-add 3 'x))
   (list 'g 'y `(f ,(make-mul 2 'y)))
   (list 'h 'v (make-add '(f v) '(g v)))))

(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(add? ex) (+ (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (mul-left ex) da)
                  (eval-function* (mul-right ex) da))]
    [(list? ex)
     (local
       ((define res (lookup-def da-fgh (first ex)))
        (define var (second res))
        (define body (last res)))
     (eval-function*
      (subst body var (eval-function* (second ex) da))
      da))]
   ))
          
    
(define e (make-add '(f 1) '(g 3)))
(check-error (eval-function* '(q 2)))
(check-expect (eval-function* 
               (make-add '(f 1) '(g 3)) da-fgh)
              (eval-exp (make-add (make-add 3 1) 
                                  (make-add 3 (make-mul 2 3)))))
      
(check-expect (eval-function*
               (make-add `(f ,(make-mul 1 3))
                         `(h ,(make-add 2 2))) da-fgh)
              (eval-exp (make-add
                         (make-add 3 (make-mul 1 3))
                         (make-add
                          (make-add 3 (make-add 2 2))
                          (make-add
                           3
                           (make-mul 2 (make-add 2 2)))))))

;; mix

;; bsl-da-all
;; - '(symbol number)   constant def
;; - '(symbol symbol bsl-da-all)

(define (lookup-con-def alldef x)
  (local ((define res (assq x alldef)))
          (if res res (error "not found"))))

(define (lookup-fun-def alldef f)
  (local ((define res (assq f alldef)))
          (if res res (error "not found"))))


(define AD0
  `((pi 3.14)
    (area r ,(make-mul 'pi (make-mul 'r 'r)))
    (vol r ,(make-mul 10 '(area r)))))

(define (eval-all exp da)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (second (lookup-con-def da exp))]
    [(add? exp)
     (+ (eval-all (add-left exp) da)
        (eval-all (add-right exp) da))]
    [(mul? exp)
     (* (eval-all (mul-left exp) da)
        (eval-all (mul-right exp) da))]
    [(list? exp)
     (local
       ((define res (lookup-fun-def da (first exp)))
        (define var (second res))
        (define body (last res)))
       (eval-all
        (subst body var
               (eval-all (second exp) da))
        da))]))
       

(check-within (eval-all 'pi AD0) 3.14 0.001)
(check-within (eval-all '(area 2) AD0)
              (eval-exp
               (make-mul 3.14 (make-mul 2 2))) 0.001)
(check-within (eval-all `(area ,(make-mul 2 4)) AD0)
              (eval-exp
               (make-mul 3.14 (make-mul 8 8))) 0.001)
(check-within (eval-all `(vol ,(make-add 'pi 1)) AD0)
              (eval-exp
               (make-mul 10
                         (make-mul
                          3.14
                          (make-mul 4.14 4.14)))) 0.001)
(define (list-of-3? lst)
  (= 3 (length lst)))
(define (list-of-2? lst)
  (= 2 (length lst)))
(define (interpreter-h sexp sl)
  (cond
    [(number? sexp) sexp]
    [(add? sexp)
     (make-add
      (interpreter-h (add-left sexp) sl)
      (interpreter-h (add-right sexp) sl))]
    [(mul? sexp)
     (make-mul
      (interpreter-h (mul-left sexp) sl)
      (interpreter-h (mul-right sexp) sl))]
    [(and (list? sexp) (list-of-3? sexp))
     (cond
       [(equal? (first sexp) '+)
        (make-add (interpreter-h (second sexp) sl)
                  (interpreter-h (third sexp) sl))]
       [(equal? (first sexp) '*)
        (make-mul (interpreter-h (second sexp) sl)
                  (interpreter-h (third sexp) sl))])]
                   
    [(and (list? sexp) (list-of-2? sexp))
     (local
       ((define res (lookup-fun-def sl (first sexp)))
        (define var (second res))
        (define body (last res)))
       (interpreter-h
        (subst
         body var
         (interpreter-h (second sexp) sl)) sl))]
    [(symbol? sexp) (second (lookup-con-def sl sexp))]
  
    [(list? sexp)
     (map (lambda (se) (interpreter-h se sl)) sexp)]
  ))

(define (interpreter sexp sl)
  (eval-all (interpreter-h sexp sl) sl))

(check-within (interpreter 'pi AD0) 3.14 0.001)
(check-within (interpreter '(area 2) AD0)
              12.56 0.001)
(check-within (interpreter '(area (+ 2 4)) AD0)
              113.04 0.001)
(check-within (interpreter '(vol (* pi 1)) AD0)
              309.59144 0.001)












(test)