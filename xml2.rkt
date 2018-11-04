#lang racket

(require test-engine/racket-tests 2htdp/batch-io 2htdp/universe 2htdp/image)

; [list of number ] [list of number] -> [list of number]
; replaces the final '() in front with end

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))



(check-expect (replace-eol-with '() '(a b)) '(a b))

(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))

(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (cross l1 l2)
  (cond
    [(empty? l1) '()]
    [else
     (append
      (permute (first l1) l2)
      (cross (rest l1) l2))]))
(define (permute n lst)
  (cond
    [(empty? lst) '()]
    [else
     (cons
      (list n (first lst))
      (permute n (rest lst)))]))

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

; [list of number] [list of number] -> [list of number]
; multiplies the corresponding items on
; hours and wages/h
; assume the two lists are of equal length
(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
      (weekly-wage (first hours) (first wages/h))
      (wages*.v2 (rest hours) (rest wages/h)))]))

; number number -> number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

(check-expect (wages*.v2 '() '()) '())
(check-within (wages*.v2 (list 5.65) (list 40)) (list 226.0) 0.001)

(define-struct employee [name ssn rate])
(define-struct work [name hours])
(define-struct wages [name wage] #:transparent)

(define (wages*.v3 l1 l2)
  (local
    ((define names (map employee-name l1))
     (define rates (map employee-rate l1))
     (define hourss (map work-hours l2)))
    (map make-wages names (wages*.v2 rates hourss))))
(check-expect (wages*.v3 `(,(employee "chi" 123 20)) `(,(work "chi" 2)))
              (list (make-wages "chi" 40)))



(define-struct phone-record [name number] #:transparent)

; a phonerecord is a structure:
; (make-phone-record string string)

(define (zip l1 l2)
  (map make-phone-record l1 l2))

(check-expect (zip (list "chi") (list 123)) 
              (list (make-phone-record "chi" 123)))


; n is one of:
; - 0
; - (add1 n)

; [list of symbol] n -> symbol
; extracts the nth symbol from l
; signals an error if there is no such symbol
(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (> n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))


(check-expect (list-pick '(a b) 1) 'b)

  (check-expect (list-pick '(a b c) 2) 'c)
  (check-error (list-pick '() 0) )
  (check-expect (list-pick (cons 'a '()) 0) 'a)
  (check-error (list-pick '() 3) )
  

(define-struct branch [left right] #:transparent)
; a tos is one of:
; -symbol
; -(make-branch tos tos)

; a direction is one of
; - 'left
; - 'right

; a list of direction is also called a path

(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (not (empty? lod))) (error "too much")]
    [(empty? lod) tos]
    [else (if (equal? 'left (first lod)) 
              (tree-pick (branch-left tos) (rest lod))
              (tree-pick (branch-right tos) (rest lod)))]))

(check-expect (tree-pick 
               (make-branch (make-branch 'c 
                                         (make-branch 'e 'f))
                            'b) (list 'left 'right))
              (make-branch 'e 'f))



;;
(define (union l1 l2)
  (remove-dup (append l1 l2)))

(define (remove-dup lst)
  (cond
    [(empty? lst) '()]
    [else (if (member? (first lst) (rest lst))
              (remove-dup (rest lst))
              (cons (first lst) (remove-dup (rest lst))))]))
(define (member? x lst)
  (cond
    [(empty? lst) #f]
    [else (or (equal? x (first lst))
              (member? x (rest lst)))]))
(check-expect (remove-dup '( 1 2 3 3 3)) '(1 2 3))

(check-expect (union '(1 2 3 3) '(2 3 4 1))
              '(2 3 4 1))


(define (intersect l1 l2)
  (cond
    [(empty? l1) '()]
    [else (if (member? (first l1) l2)
              (cons (first l1)
                    (intersect (rest l1) l2))
              (intersect (rest l1) l2))]))
(check-expect (intersect '(1 2 3) '(2 3 4 5)) '(2 3))

(define (merge l1 l2)
  (sort (append l1 l2)))
(define (sort lst)
  (cond
    [(empty? lst) '()]
    [else (insert (first lst) (sort (rest lst)))]))
(define (insert n lst)
  (cond
    [(empty? lst) (list n)]
    [else (if (> n (first lst))
              (cons (first lst) (insert n (rest lst)))
              (cons n (insert (first lst) (rest lst))))]))
(check-expect (merge '(1 2 3 4 4) '(4 5 2 1))
              (list 1 1 2 2 3 4 4 4 5))


(define (take lst n)
  (cond
    [(empty? lst) '()]
    [(zero? n) '()]
    [else (cons (first lst) (take (rest lst) (sub1 n)))]))

(check-expect (take '(2 4 3) 2) '(2 4))
(check-expect (take '(2 3 4) 4) '(2 3 4))
                  
(define (drop lst n)
  (cond
    [(empty? lst) '()]
    [(zero? n) lst]
    [else (drop (rest lst) (sub1 n))]))

(check-expect (drop '(4 3 2 1) 2) '(2 1))
(check-expect (drop '(2 1) 3) '())

(define AS-LIST (read-lines "words.txt"))
(define SIZE (length AS-LIST))

(define LETTERS (string->list "abcdefghijklmnopqrstuvwxyz"))


; an hm-word is a (list of letter or "_"]
; interpretation "_" represents a letter to be guessed



; hm-word n -> string
; runs a simplistic hangman gme, produces the current state

(define (compare-word w s ke-sym)
  (cond
    [(empty? s) '()]
    [else (if (and (equal? (first s) #\_)
                   (equal? (first w) ke-sym))
              (cons ke-sym (rest s))
              (cons (first s) (compare-word (rest w) (rest s) ke-sym)))]))
  
(define (play the-pick time-limit)
  (local ((define the-word (string->list the-pick))
          (define the-guess (make-list (length the-word) #\_))
          ; hm-word -> hm-word
          (define (do-nothing s) s)
          ; hm-word keyevent -> hm-word
          (define (checked-compare current-status ke)
            (local ((define ke-sym (first (string->list ke))))
              (if (member? ke-sym LETTERS)
                  (compare-word the-word current-status ke-sym)
                  current-status))))
    (list->string
     (big-bang the-guess; hm-word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key checked-compare]))))
; hm-word -> image
(define (render-word w)
  (text (list->string w) 22 "black"))
         
;(play (list-ref AS-LIST (random SIZE)) 10)

(define el (list
            (make-employee "abel" 123 20)
            (make-employee "chi" 234 10)
            (make-employee "freya" 890 5)
            (make-employee "zeus" 876 25)))
(define tl (list
            (make-work "abey" 20)
            (make-work "freya" 30)
            (make-work "zeus" 23)
            (make-work "chi" 20)))

(define (wages*.v4 elist tlist)
  (cond
    [(empty? elist) '()]
    [else (local ((define emp (first elist))
                  (define name (employee-name emp))
                  (define rate (employee-rate emp))
                  (define time (find-time name tlist)))
            
            (cons (make-wages name (* time rate)) 
                  (wages*.v4 (rest elist) tlist)))]))

(define (find-time n lst)
  (cond
    [(empty? lst) (error 'find-time "name not found")]
    [else (if (equal? n (work-name (first lst)))
              (work-hours (first lst))
              (find-time n (rest lst)))]))

(check-error (find-time "abel" tl))
(check-expect (find-time "freya" tl) 30)

(check-error (wages*.v4  el tl))
(check-expect (wages*.v4 (rest el) (rest tl))
              (list (wages "chi" 200) (wages "freya" 150) (wages "zeus" 575)))
                  

(define (alien elist tlist)
  (map (lambda (x) (find-time x elist)) tlist))

(check-error (alien el tl))

(define (combine-value coef val)
  (map * coef val))

(check-expect (combine-value '(1 2 3) '(9 8 7)) '(9 16 21))

; [list of string] -> [list of string]
; picks a random non-identity arrangement of names
(define P (list "louise" "jane" "laura" "dana" "mary"))

(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))

; [list of string] -> [list of [list of string]]
; returns all possible permutations of names
(define (arrangements names)
  (cond
    [(empty? names) (list '())]
    [else (insert-everywhere/in-all-combinations
           (first names)
           (arrangements (rest names)))]))

(define (insert-everywhere/in-all-combinations name loc)
  (cond
    [(empty? loc) '()]
    [else (append (insert-everywhere name (first loc))
                  (insert-everywhere/in-all-combinations name (rest loc)))]))

(define (insert-everywhere name lon)
  (insert-everywhere-helper name lon '()))

(define (insert-everywhere-helper name lon1 lon2)
  (cond
    [(empty? lon1) (list (append '() (list name) lon2))]
    [else (cons (append lon1 (list name) lon2)
                (insert-everywhere-helper
                 name
                 (first (move-left (list lon1 lon2)))
                 (second (move-left (list lon1 lon2)))))]))

(check-expect (insert-everywhere-helper 1 (list 2 3) '())
              (list (list 2 3 1) (list 2 1 3) (list 1 2 3)))

(define (move-left lon)
  (list (drop-last (first lon))
        (cons (last (first lon)) (second lon))))

(define (drop-last lon)
  (drop-right lon 1))

(check-expect (drop-last (list 1 2 3)) (list 1 2))
(check-expect (move-left (list (list 1 2) (list 3)))
                         (list (list 1) (list 2 3)))


   

; [nelist of x] -> x
; returns a random item from the list
(define (random-pick l)
  (list-ref l (random (length l))))

; [list of string] [list of [list of string]]
; ->
; [list of [list of string]]
; produces the lits of those lists in ll that do
; not agree with names at any place
(define (non-same names ll)
  (local ((define b (benchmarks)))
    (filter (lambda (x) (not (my-match? x b))) ll)))

(check-expect (non-same P (list (list "louise" 1 2 3) (list 1 2 3 4)))
              (list (list 1 2 3 4)))

(define (my-match? x patterns)
  (cond
    [(empty? patterns) #f]
    [else 
     (if (compare x (first patterns))
         #t
         (my-match? x (rest patterns)))]))


(define (compare x lst)
  (cond
    [(empty? lst) #f]
    [else
     (if (equal? (first x) (first lst))
         #t (compare (rest x) (rest lst)))]))
  
(define (replace posn with)
  (for/list ([i 4])
    (if (= i posn)
        with
        "_")))

(define (benchmarks)
  (for/list ([i 4])
    (replace i (list-ref P i))))

;;

(define (dnaprefix lst srch)
  (cond
    [(and (empty? lst) (empty? srch)) #t]
    [(empty? srch) #t]
    [(and (empty? lst) (not (empty? srch))) #f]
    [else
     (if (equal? (first lst) (first srch))
         (dnaprefix (rest lst) (rest srch))
         #f)]))

(check-expect (dnaprefix '(a g t c a a) '(a g)) #t)

(define (dnadelta lst srch)
  (cond 
    [(and (dnaprefix lst srch)
          (> (length lst) (length srch)))
     (list-ref lst (length srch))]
    [else #f]))

(check-expect (dnadelta '(a g t c a a) '(a g)) 't)


; an sexpr is one of
; atom
; [list of sexpr]
; an atom is one of
; number
; string
; symbol

(define (sexp=? s1 s2)
  (cond
    [(atom? s1)
     (if (equal? s1 s2) #t #f)]
    [else
     (if (atom? s2) #f
         (andmap sexp=? (rest s1) (rest s2)))]))
(check-expect (sexp=? '("str") '("str")) #t)
(define (and-map lst)
  (cond
    [(empty? lst) #t]
    [else (if (false? (first lst))
              #f
              (and-map (rest lst)))]))
                     
     (check-expect (and-map '()) #t)
(check-expect (and-map '(#t #f)) #f)

(define (atom? s)
  (or (number? s) (string? s) (symbol? s) (empty? s)))
          
     

;;
(define-struct db [schema content])
; a db is a structure
; (make-db scheme content]

; a schema is a [list of spec]
; a spec is a [list label predicate]
; a label is a string
; a predicate is a [any->boolean]

; a piece of content is a [list of row]
; a row is a [list of cell]
; a cell is any
; constraint cells do not contain functions

; integrity constraint in (make-db sch con]
; for every row in con
; i1 its length is the same as sch
; i2 its ith cell satisfies the ith predicate in sch

(define school-schema
  `(("name" ,string?)
    ("age" ,integer?)
    ("present" ,boolean?)))

(define school-content
  `(("alice" 35 #t)
    ("bob" 25 #f)
    ("carol" 30 #t)
    ("dave" 32 #f)))
(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(("present" ,boolean?)
    ("description" ,string?)))

(define presence-content
  `((#t "presence")
    (#f "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))


; db -> boolean
; do all rows in db satisfy i1 and i2

(check-expect (integrity-check school-db) #t)
(check-expect (integrity-check presence-db) #t)

(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define width (length schema))
          ; row -> boolean
          ; does row satisfy i1 and i2
          (define (row-integrity-check row)
            (and 
             (= (length row) width)
             (andmap (lambda (s c) [(second s) c])
                     schema
                     row))
          ))
    (andmap row-integrity-check content)))

;; db [list of label] -> db
; retains a column from db if its label is in labels
(define projected-content
  `(("alice" #t)
    ("bob" #f)
    ("carol" #t)
    ("dave" #f)))

(define projected-schema
  `(("name" ,string?) ("present" ,boolean?)))

(define projected-db
  (make-db projected-schema projected-content))

(check-expect (project school-db '("name" "present")) projected-db)

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          
          ; spec ->boolean
          ; does this spec belong to the new schema
          (define (keep? c) (member? (first c) labels))
          
          ; row -> row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c)
                     (if m (cons cell c) c))
                   '()
                   row mask))
          
          (define mask (map keep? schema)))
          
    (make-db (filter keep? schema)
             (map row-project content))))

(check-expect
 (db-content (project school-db '("name" "present")))
 projected-content)


(define (andmap2 f l1 l2)
  (define combine (map list l1 l2))
  (map (lambda (x) (apply f x)) combine))

; [list of 1string] n -> [list of string]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (list->string (take s n)) (bundle (drop s n)n ))]))

; [list of x] n -> [list of x]
; keeps the first n items from l if possible or everything

; [list of x] n -> [list of x]
; removes the first n items from l if possible or everything

(check-expect (bundle (string->list "abcdefg") 3)
              (list "abc" "def" "g"))
  
;;

(define (list->chunks s n)
  (cond
    [(empty? s) '()]
    [else (cons (take s n) (list->chunks (drop s n) n))]))

(define (bundle2 s n)
  (map list->string (list->chunks s n)))
(check-expect (bundle2 (string->list "abcdefg") 3)
              (bundle (string->list "abcdefg") 3))

(define (partition-h s n1 n)
  (cond
    [(>= (+ n1 n) (string-length s)) (list (substring s n1))]
    [else (cons (substring s n1 (+ n1 n))
                (partition-h s (+ n1 n) n))]))

(define (partition s n)
  (partition-h s 0 n))
                       

(check-expect (partition "abcdefg" 3) (bundle (string->list "abcdefg") 3))

(define (sort< lst)
  (cond
    [(empty? lst) '()]
    [else (insert (first lst) (sort< (rest lst)))]))
(define (insert2 n lst)
  (cond
    [(empty? lst) '(n)]
    [else (if (> n (first lst))
              (cons (first lst (insert2 n (rest lst))))
              (cons n (insert2 (first lst) (rest lst))))]))

(define (quick-sort<2 alon)
  (cond
    [(< (length alon) 10) (sort< alon)]
    [else (quick-sort< alon)]))

; [list of number] -> [list of number]
; produces a sorted version of alon
; assume the number sare all distinct

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot))))]))

; [list of number] number -> [list of number]
(define (largers alon n)
  (filter (lambda (x) (not (< x n))) alon))
 
; [list of number] number -> [list of number]
(define (smallers alon n)
  (filter (lambda (x) (< x n)) alon))
  


;;;
(define SMALL 4) ; a size measure in terms of pixels 
 
(define small-triangle (triangle SMALL 'outline 'red))
 
; Number -> Image
; generative creates Sierpinski Δ of size side by generating
; one for (/ side 2) and placing one copy above two copies
 
(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))
 
(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized (beside half-sized half-sized)))]))



(test)