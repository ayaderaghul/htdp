#lang racket
(require lang/htdp-advanced test-engine/racket-tests)
;; chapt IV intertwined data.
;; 20. iterative refinement

;; https://htdp.org/2018-01-06/Book/part_four.html#%28part._sec~3arefine-funcs%29

(define-struct file2 [name size content])
; a file is a structure
; (make-file string n string)

(define-struct dir2 [name dirs files])
; a dir2 is a structure
; (make-dir2 string dir* file*]

; a dir* is one of
; '()
; (cons dir2 dir*)

; a file* is one of
; '()
; (cons file file*)

(define bt3
  (make-dir2
   "ts" (list
         (make-dir2
          "text" '()
          (list
           (make-file2 "part1" 99 "")
           (make-file2 "part2" 52 "")
           (make-file2 "part3" 17 "")))
         (make-dir2
          "libs"
          (list
           (make-dir2 "code"
                      '()
                      (list
                       (make-file2 "hang" 8 "")
                       (make-file2 "draw" 2 "")))
           (make-dir2 "docs"
                      '()
                      (list
                       (make-file2 "read" 19 ""))))
          '()))
   (list (make-file2 "read" 10 ""))))


;;; exercise 342 
(define (find2 n lod dname)
  (cond
    [(empty? lod) (list #f)]
    [else 
     (first 
      (empty->false   ;; if all fails, it returns '() -> #f
       (filter not-false-end?   ;; filter out the dead ends
               (map (lambda (d) (cons dname (find-h n d))) lod))))]))

(define (empty->false lst)
  (if (empty? lst) (list (list #f)) lst))

(define (find-h n d)    ;; started out as helper, becomes main function
  (cond
    [(member? n (map file2-name (dir2-files d))) (list (dir2-name d) n)]
    [else (find2 n (dir2-dirs d) (dir2-name d))]))

(check-expect (find-h "hang" bt3) '("ts" "libs" "code" "hang"))


; generalise find-h
(define (find-h2 n d) 
  (local
    ((define res (find2 n (dir2-dirs d) (dir2-name d))))
    (if (member? n (map file2-name (dir2-files d))) 
        (list (list (dir2-name d) n) res)
        res)))

(define (not-false-end? lst)
  (not (false? (last lst))))

(check-expect (find-h2 "read" bt3)
              '(("ts" "read") ("ts" "libs" "docs" "read")))


;;; 343 
(define (ls-R lod dname)
  (cond
    [(empty? lod) '()]
    [else 
     (local ((define res 
               (map ls-h lod)))
       (add-names dname (unlist res)))]))
       ;(map (lambda (n) (add-names dname n)) (unlist res)))]))

(define (ls-h dtree)    ;; started out as a helper
  (append               ;; becomes main function
   (map (lambda (n) (add-names (dir2-name dtree) n)) 
        (map file2-name (dir2-files dtree)))
   (ls-R (dir2-dirs dtree) (dir2-name dtree))))

(check-expect (ls-h bt3)
              '(("ts" "read")
                ("ts" "text" "part1")
                ("ts" "text" "part2")
                ("ts" "text" "part3")
                ("ts" "libs" "code" "hang")
                ("ts" "libs" "code" "draw")
                ("ts" "libs" "docs" "read")))

(define (add-names dname s-or-los) ;; cons the name of current dir to objects
  (cond                            ;; that can be a file name or a list
    [(string? s-or-los) (list dname s-or-los)]
    [(list? s-or-los)
     (map (lambda (n) (cons dname n)) s-or-los)]))
   
  
(define (unlist lol)  ;; too many parentheses -> un-bracket one level
  (cond
    [(empty? lol) '()]
    [else (append (first lol) (unlist (rest lol)))]))


;; 344 
(define (find-all name dtree)
  (local
    ((define lop (ls-h dtree)))
    (filter (lambda (p) (equal? name (last p))) lop)))
     


(test)