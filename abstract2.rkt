#lang racket
(require test-engine/racket-tests)

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
 
 
 
 
(test)