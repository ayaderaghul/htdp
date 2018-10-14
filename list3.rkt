#lang racket

(require test-engine/racket-tests)
(define WPH 12)
; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (if (> h 100)
      "too many hours"
      (* WPH h)))


;;;

(define (f->c t)
  (* 5/9 (- t 32)))
(define (f->cs ts)
  (cond
    [(empty? ts) '()]
    [else (cons (f->c (first ts)) (f->cs (rest ts)))]))

(define (dollar->euro dollar)
  (* 0.86 dollar))
(define (convert-euro ds)
  (cond
    [(empty? ds) '()]
    [else (cons (dollar->euro (first ds)) (convert-euro (rest ds)))]))

(define (x->euro x er)
  (* er x))
(define (convert-euro* ds er)
  (cond
    [(empty? ds) '()]
    [else (cons (x->euro (first ds) er) (convert-euro* (rest ds)))]))

(define (robot->r2d2 toy)
  (if (= toy "robot") "r2d2" toy))
(define (subst-robot toys)
  (cond
    [(empty? toys) '()]
    [else (cons (robot->r2d2 (first toys)) (subst-robot (rest toys)))]))

(define (old->new x old new)
  (if (= x old) new x))
(define (subst-toys toys old new) 
  (cond
    [(empty? toys) '()]
    [else (cons (old->new (first toys) old new) (subst-toys (rest toys)))]))


;;;

(define-struct work [employee rate hours] #:transparent)
(define-struct paycheck [employee amount] #:transparent)
; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
 
(check-within
  (wage*.v2 (cons (make-work "Robby" 11.95 39) '()))
  (cons (* 11.95 39) '())
  0.001)
 
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

(define (wage*.v3 low)
  (cond
    [(empty? low) '()]
    [else (cons (wage.v3 (first low))
                (wage*.v3 (rest low)))]))

(define (wage.v3 w)
  (make-paycheck
   (work-employee w)
   (* (work-rate w) (work-hours w))))
(check-within (wage*.v3 (cons (make-work "Chi" 10.2 8)
                              (cons (make-work "Angela" 9.5 7)
                                    '())))
              (list (make-paycheck "Chi" (* 10.2 8))
                    (make-paycheck "Angela" (* 9.5 7)))
              0.001)

;;;

(define-struct posn [x y] #:transparent)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop)) (sum (rest lop)))]))

(check-expect (sum (list (make-posn 1 2) (make-posn 2 3))) 3)

(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else (cons (make-posn (posn-x (first lop))
                           (add1 (posn-y (first lop))))
                (translate (rest lop)))]))
(check-expect (translate (list (make-posn 1 2) (make-posn 2 3)))
              (list (make-posn 1 3) (make-posn 2 4)))
                         
(define (legal? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))

(define (legal lop)
  (cond
    [(empty? lop) '()]
    [else (if (legal? (first lop))
              (cons (first lop) (legal (rest lop)))
              (legal (rest lop)))]))

(check-expect (legal (list (make-posn 0 2) (make-posn 3 4)))
              (list (make-posn 0 2) (make-posn 3 4)))
(check-expect (legal (list (make-posn -1 10) (make-posn 5 300)))
              '())

;;;

(define-struct phone [area switch four] #:transparent)
; a phone is a structure
; (make-phone three three four)
; a three is a number bw 100 and 999
; a four is a number bw 1000 and 9999

(define (713->281 x)
  (if (= x 713) 281 x))
(define (edit p)
  (make-phone (713->281 (phone-area p))
              (phone-switch p)
              (phone-four p)))

(define (replace lop)
  (cond
    [(empty? lop) '()]
    [else (cons (edit (first lop)) (replace (rest lop)))]))

(check-expect (replace (list (make-phone 111 222 3333)
                             (make-phone 713 222 3333)))
              (list (make-phone 111 222 3333)
                    (make-phone 281 222 3333)))
                    
(require 2htdp/batch-io)
;;;;
; LLS -> List-of-numbers
; determines the number of words on each line 

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

(check-expect (words-on-line (read-words/line "ttt"))
              (list 5 5 3 2))
(check-expect (file-statistic "ttt")
              (list 5 5 3 2))
              
; 1String -> String
; converts the given 1String to a 3-letter numeric String
 ; 0 to 256
(require lang/htdp-advanced)

(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))


(define INPUT (read-words/line "ttt"))
(define OUTPUT (read-file "ttt"))
(define (collapse lls)
  (cond
    [(empty? (rest lls)) (make-sentence (first lls))]
    [else (string-append (make-sentence (first lls))
                         "\n"
                         (collapse (rest lls)))]))

(define (make-sentence los)
  (cond
    [(empty? (rest los)) (first los)]
    [else (string-append (first los) " " (make-sentence (rest los)))]))

(check-expect (collapse INPUT) OUTPUT)
(check-expect
 (read-file (write-file "test" (collapse (read-words/line "ttt"))))
 (read-file "ttt"))

;;;

(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [else (cons (remove-art/line (first lls))
                (remove-articles (rest lls)))]))


(define (remove-art/line los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) ART)
              (remove-art/line (rest los))
              (cons (first los)
                    (remove-art/line (rest los))))]))

(define ART (list "a" "an" "the"))


;;;
(define IN2 (map explode (read-lines "ttt")))
(define (letter->number lls)
  (cond
    [(empty? lls) '()]
    [else (cons (letter->number/line (first lls))
                (letter->number (rest lls)))]))
(define (letter->number/line lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (cons (encode-letter (first lo1s))
                (letter->number/line (rest lo1s)))]))

(define DATA (read-words/line "ttt"))
(define (file-stats file-name)
  (list
   (length (explode (read-file file-name)))
   (word-count DATA)
   (length DATA)))
   
 
   
(define (word-count lls)
  (cond
    [(empty? lls) 0]
    [else (+ (length (first lls))
             (word-count (rest lls)))]))

(check-expect (file-stats "ttt") (list 75 15 4))

;; a matrix is one of:
; - (cons row '())
; - (cons row matrix)

; constraint: all rows in matrix are of the same length

; a row is one of:
; - '()
; - (cons number row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

(define (first* lln)
  (cond
    [(empty? lln) '()]
    [else (cons (first (first lln)) (first* (rest lln)))]))
(define (rest* lln)
  (cond
    [(empty? lln) '()]
    [else (cons (rest (first lln)) (rest* (rest lln)))]))





(test)
                                   