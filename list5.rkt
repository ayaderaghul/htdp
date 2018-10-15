#lang racket
(require 2htdp/batch-io lang/htdp-advanced)

(define LOCATION "~/usr/share/dict/words")
(define AS-LIST (read-lines "words.txt"))
(define DICT (take AS-LIST (/ (length AS-LIST) 4)))
; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [else
     (+ 
      (if (string=? letter (first (explode (first dict)))) 1 0)
      (starts-with# letter (rest dict)))]))
;(starts-with# "a" AS-LIST)
(define-struct lcount [letter count])

(define (count-by-letter-helper lol dict)
  (cond
    [(empty? lol) '()]
    [else (cons (make-lcount (first lol) (starts-with# (first lol) dict))
                (count-by-letter-helper (rest lol) dict))]))
(define (count-by-letter dict)
  (count-by-letter-helper LETTERS dict))


(define COUNT-BY-LETTER
  (list
   (make-lcount "a" 24370)
   (make-lcount "b" 18969)
 (make-lcount "c" 32232)
 (make-lcount "d" 19366)
 (make-lcount "e" 14017)
 (make-lcount "f" 13599)
 (make-lcount "g" 10972)
 (make-lcount "h" 14862)
 (make-lcount "i" 13626)
 (make-lcount "j" 2533)
 (make-lcount "k" 3545)
 (make-lcount "l" 10368)
 (make-lcount "m" 19266)
 (make-lcount "n" 13749)
 (make-lcount "o" 13236)
 (make-lcount "p" 35383)
 (make-lcount "q" 2926)
 (make-lcount "r" 18431)
 (make-lcount "s" 43939)
 (make-lcount "t" 21218)
 (make-lcount "u" 22902)
 (make-lcount "v" 5452)
 (make-lcount "w" 9727)
 (make-lcount "x" 433)
 (make-lcount "y" 1245)
 (make-lcount "z" 1185)))

(define (sort-lcount lolc)
  (cond
    [(empty? (rest lolc)) lolc]
    [else (insert-lcount (first lolc) (sort-lcount (rest lolc)))]))
(define (insert-lcount lc lolc)
  (cond
    [(empty? lolc) '()]
    [else (if (>= (lcount-count lc) (lcount-count (first lolc)))
              (cons lc lolc)
              (cons (first lolc) (insert-lcount lc (rest lolc))))]))
(check-expect (insert-lcount (make-lcount "a" 10)
                             (list (make-lcount "b" 20)
                                   (make-lcount "c" 30)))
              (list (make-lcount "b" 20)
                    (make-lcount "c" 30)
                    (make-lcount "a" 10)))
(check-expect (sort-lcount (list (make-lcount "a" 10)
                                 (make-lcount "b" 20)
                                 (make-lcount "c" 30)))
              (list (make-lcount "c" 30)
                    (make-lcount "b" 20)
                    (make-lcount "a" 10)))
  
(define (most-frequent dict)
  (first (sort-lcount COUNT-BY-LETTER)))

(define (collect letter dict)
  (cond
    [(empty? dict) '()]
    [else (if (string=? letter (first (explode (first dict))))
              (cons (first dict) (collect letter (rest dict)))
              (collect letter (rest dict)))]))

(define (words-by-first-letter lolc dict)
  (cond
    [(empty? lolc) '()]
    [else (cons (collect (first lolc) dict)
                (words-by-first-letter (rest lolc) dict))]))
(define WBL (words-by-first-letter LETTERS DICT))

;; sort list of dictionary based on its length
;; use insert as helper

