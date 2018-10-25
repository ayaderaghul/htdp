#lang racket
(require lang/htdp-advanced test-engine/racket-tests)

; [list of x ] -> [list of [list n x]]
; pairs each item in lx w its index

(check-expect (enumerate '(a b c))
              '((1 a) (2 b) (3 c)))

(define (enumerate lx)
  (for/list ([x lx] [ith (length lx)])
    (list (+ ith 1) x)))


; [list of x] [list of y] -> [list of [list x y]]
; generates all pairs of items from l1 and l2

;(check-satisfied (cross '(a b c) '(1 2))
;                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
  (for*/list ([x1 l1] [x2 l2])
    (list x1 x2)))

; [list of x] -> [list of [list of x]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else (for*/list ([item w]
                      [arrangement-without-item
                       (arrangements (remove item w))])
            (cons item arrangement-without-item))]))


; [list of x] -> boolean
(define (all-words-from-rat? w)
  (and (member? (explode "rat") w)
       (member? (explode "art") w)
       (member? (explode "tar") w)))

(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)


; n -> sequence?
; construct the infinite sequence of natural numbers,
; starting from n
(define (enumerate.v2 lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))

; n n n ->sequence?
; construct the finite sequence of natural numbers:
; start
; (+ start step]
; (+ start step step]
; ...
; until the number exceeds end
(check-expect (sum-evens 2 ) 0)
(check-expect (sum-evens 4) 2)
(define (sum-evens n)
  (for/sum ([i (in-range 0 n 2)]) i))

(define (convert-euro lod)
  (for/list ([i lod])
    (* i 0.9)))

(define (build-list1 n)
  (for/list ([i n])
    i))
(check-expect (build-list1 3) (list 0 1 2))
(define (build-list2 n)
  (for/list ([i n])
    (+ i 1)))

(check-expect (build-list2 3) (list 1 2 3))
(define (build-list3 n)
  (for/list ([i n])
    (/ 1 (+ i 1))))
(check-expect (build-list3 3) (list 1 1/2 1/3))

(define (build-list4 n)
  (for/list ([i n])
    (* i 2)))
(define (create-identity-matrix n)
  (local ((define (create-row 1-at size)
            (for/list ([i size])
              (if (= 1-at i) 1 0))))
    (for/list ([i n])
      (create-row i n))))
(check-expect (create-identity-matrix 1) (list (list 1)))
(check-expect (create-identity-matrix 3)
              (list (list 1 0 0)
                    (list 0 1 0)
                    (list 0 0 1)))

(define (tabulate f n)
  (for/list ([i n])
    (f i)))

(define (contains? l1 l2)
  (andmap (lambda (in-l1) (member? in-l1 l2)) l1))

(check-expect (contains? (list 1 2) (list 2 3 1)) #t)
(check-expect (contains? (list 1) (list 2 3 4)) #f)

(define (find-name name lon)
  (for/or ([i lon])
    (contains? (explode name) (explode i))))

(check-expect (find-name "chi" (list "chit")) #t)
(check-expect (find-name "chi" (list "a")) #f)

(define (good-length? lon len)
  (for/and ([i lon])
    (<= (string-length i) len)))

(check-expect (good-length? (list "abc" "ab") 3) #t)
(check-expect (good-length? (list "ab" "abcd") 3) #f)

; a [non empty list x] is one of:
; - (cons x '())
; - (cons x [non empty list x])

; [non empty list x] -> x
; retrieves the last item of a ne-l
(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))

(define-struct layer [color doll])
; an rd (russian doll] is one of:
; - "doll"
; - (make-layer string rd]

; rd -> n
; how many dolls are a part of an-rd

;[list of posn] -> [lst of posn]
; moves each object right by delta-x pixels

(define input `(,(make-posn 1 1) ,(make-posn 10 14)))
(define e-output `(,(make-posn 4 1) ,(make-posn 13 14)))


;;
(define-struct child [father mother name date eyes])
; a child is a structure
; an ft (family tree] is one of
; (make no parent]
; (make-child child child string n string]


(define-struct no-parent [])
(define NP (make-no-parent))
(define Bettina
  (make-child (make-no-parent)
              (make-no-parent)
              "Bettina" 1926 "green"))
(define Carl
  (make-child NP NP "Carl" 1926 "green"))

(define Adam
  (make-child Carl Bettina 
            "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #f]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree))
              )]))

(check-expect (blue-eyed-child? Carl) #f)
(check-expect (blue-eyed-child? Gustav) #t)

; FT -> number
; count the number of child structures in the tree

(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else
     (+ 1
        (count-persons (child-father an-ftree))
        (count-persons (child-mother an-ftree)))]))
  
; FT -> number
; average age

(define (sum-age an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (child-date an-ftree)
             (sum-age (child-father an-ftree))
             (sum-age (child-mother an-ftree)))]))

(define (average-age an-ftree)
  (/ (sum-age an-ftree) (count-persons an-ftree)))

(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (append (list (child-eyes an-ftree))
                  (eye-colors (child-father an-ftree))
                  (eye-colors (child-mother an-ftree)))]))

; ft-> boolean
; seek blue eye children, except the current one
(check-expect (blue-eyed-child? Eva) #t)
(check-expect (blue-eyed-ancestor? Eva) #f)
(check-expect (blue-eyed-ancestor? Gustav) #t)

(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #f]
    [else
     (or (blue-eyed-child? (child-father an-ftree))
         (blue-eyed-child? (child-mother an-ftree)))]))

; an ff (family forest] is one of:
; '()
; (cons ft ff]
; interpretation a family forest represents several
; families (say, a town{ and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

;; ff -> boolean
; does the forest contain any child w "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #f)
(check-expect (blue-eyed-child-in-forest? ff2) #t)
(check-expect (blue-eyed-child-in-forest? ff3) #t)

(define (blue-eyed-child-in-forest? a-forest)
  (cond
    [(empty? a-forest) #f]
    [else
     (or (blue-eyed-child? (first a-forest))
         (blue-eyed-child-in-forest? (rest a-forest)))]))

; [list of ftree] -> boolean
(define (blue-eyed-child-in-forest?2 lof)
  (ormap blue-eyed-child? lof))
(check-expect (blue-eyed-child-in-forest?2 ff1)
              (blue-eyed-child-in-forest? ff1))

(define (average-age2 ff)
  (/ (apply + (map sum-age ff))
     (apply + (map count-persons ff))))


;; s expression

; an s-expr is one of
; atom
; sl

; an atom is one of
; number
; string
; symbol

; an sl is one of
; '(]
; (cons s-expr sl]

(define (atom? x)
  (or (number? x) (string? x) (symbol? x)))



(check-expect (count-se 'world 'hello) 0)
(check-expect (count-se '(world hello) 'hello) 1)
(check-expect (count-se '(((world) hello) hello) 'hello) 2)
; s-expr symbol -> n
; counts all occurrences of sy in sexp


; for intertwined data definitions
; create one func template for each data def
(define (count-se sexp sy)
  (cond
    [(atom? sexp) (count-atom sexp sy)]
    [else (count-sl sexp sy)]))

; sl symbol -> n
; counts all occurrences of sy in sl
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count-se (first sl) sy)
        (count-sl (rest sl) sy))]))

; atom symbol -> n
; counts all occurrences of sy in at
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

(define (count-se2 sexp sy)
  (local
    ((define (count-sl sl)
       (cond
         [(empty? sl) 0]
         [else
          (+ (count-se2 (first sl) sy)
             (count-sl (rest sl)))]))
     (define (count-atom at)
       (cond
         [(number? at) 0]
         [(string? at) 0]
         [(symbol? at) (if (symbol=? at sy) 1 0)])))
  (cond
    [(atom? sexp) (count-atom sexp)]
    [else (count-sl sexp)])))

(check-expect (count-se2 'world 'hello)
              (count-se 'world 'hello))
(check-expect (count-se2 '(world hello) 'hello)
              (count-se '(world hello) 'hello))
(check-expect (count-se2 '(((world) hello) hello) 'hello)
              (count-se '(((world) hello) hello) 'hello))

;; depth
; sexp -> number
(define (depth sexp)
  (cond
    [(atom? sexp) 1]
    [else (depth-sl sexp)]))

(define (depth-sl sl)
  (cond
    [(empty? sl) 0]
    [else (+ (depth (first sl)) (depth-sl (rest sl)))]))

(define (depth* sexp)
  (add1 (depth sexp)))

; substitute
; sexp symbol symbol -> sexp
(define (substitute sexp sy1 sy2)
  (cond
    [(atom? sexp) (subst-atom sexp sy1 sy2)]
    [else (subst-sl sexp sy1 sy2)]))
(define (subst-atom sexp sy1 sy2)
  (if (equal? sexp sy1) sy2 sexp))
(define (subst-sl sl sy1 sy2)
  (cond
    [(empty? sl) '()]
    [else (cons (substitute (first sl) sy1 sy2)
                (subst-sl (rest sl) sy1 sy2))]))


;; s expression
; an s-expr is one of
; number or string or symbol
; [list of sexp]

; '()
; (cons s-expr sl)

(define (count-se3 sexp)
  (cond
    [(atom? sexp) 1]
    [else (cond
            [(empty? sexp) 0]
            [else (+ (count-se3 (first sexp))
                     (count-se3 (rest sexp)))])]))

;; sexpr
;; number or string or symbol or '()
;; [list of sexp]
(define (count-se4 sexp)
  (cond
    [(atom? sexp) 1]
    [(empty? sexp) 0]
    [else (+ (count-se4 (first sexp))
             (count-se4 (rest sexp)))]))
            
;;;

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; a bt (binarytree] is one of:
; - none
; - (make-node number symbol bt bt]

(define bt1 (make-node 15 'd NONE (make-node 24 'i NONE NONE)))
(define bt2 (make-node 15 'd (make-node 87 'h NONE NONE) NONE))

(define bt0
  (make-node 63 'a
             (make-node 29 'b
                        (make-node 15 'c
                                   (make-node 10 'd
                                              NONE NONE)
                                   (make-node 24 'e
                                              NONE NONE))
                        NONE)
             (make-node 89 'f
                        (make-node 77 'g NONE NONE)
                        (make-node 95 'h
                                   NONE
                                   (make-node 99 'i
                                              NONE NONE)))))

; number bt -> boolean
(define (contains-bt? num bt)
  (cond
    [(no-info? bt) #f]
    [else (or (= num (node-ssn bt))
              (contains-bt? num (node-left bt))
              (contains-bt? num (node-right bt)))]))
(check-expect (contains-bt? 77 bt0) #t)
(check-expect (contains-bt? 0 bt0) #f)

(define (search-bt num bt)
  (cond
    [(= num (node-ssn bt)) (node-name bt)]
    [(contains-bt? num (node-left bt))
     (search-bt num (node-left bt))]
    [(contains-bt? num (node-right bt))
     (search-bt num (node-right bt))]
    [else #f]))

(check-expect (search-bt 89 bt0) 'f)
(check-expect (search-bt 0 bt0) #f)

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (append
      (inorder (node-left bt))
      (list (node-ssn bt))
      (inorder (node-right bt)))]))


(define (search-bst num bt)
  (cond
    [(= num (node-ssn bt)) (node-name bt)]
    [(< num (node-ssn bt))
     (search-bt num (node-left bt))]
    [(> num (node-ssn bt))
     (search-bt num (node-right bt))]
    [else #f]))
                        
(check-expect (search-bst 89 bt0) 'f)
(check-expect (search-bst 0 bt0) #f)                       

(define new-node (make-node 40 'j NONE NONE))

(define (create-bst bst n s)
  (cond
    [(no-info? bst) (make-node n s NONE NONE)]
    [(< n (node-ssn bst))
     (make-node (node-ssn bst)
                (node-name bst)
                (create-bst (node-left bst) n s)
                (node-right bst))]
    [(> n (node-ssn bst))
     (make-node (node-ssn bst)
                (node-name bst)
                (node-left bst)
                (create-bst (node-right bst) n s))]))

; [list of [list number symbol]] -> bst

(define (create-bst-from-list lst)
  (foldr (lambda (nxt init)
           (create-bst init (first nxt) (second nxt)))
         (make-no-info)
         lst))

(define bst-list
  '((99 o)
    (77 l)
    (24 i)
    (10 h)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))

  
(check-expect (inorder (create-bst-from-list bst-list))
              (list 10 15 24 29 63 77 89 95 99))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute2 '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
 
(define (substitute2 sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else (map (lambda (s) (substitute s old new)) sexp)]))

; a dir (directory) is one of
; '()
; (cons file dir)
; (cons dir dir)
; a file is a string

(define dt0
  (list "read"
        (list "part1" "part2" "part3")
        (list (list "hang" "draw")
              (list "read"))))

(define (how-many dt)
  (cond
    [(empty? dt) 0]
    [(string? dt) 1]
    [else (apply + (map how-many dt))]))
                  
(check-expect (how-many dt0) 7)

(define-struct dir0 [name content size readability])
; a dir is a struct
; (make-dir string lofd)

; an lofd (list of files and dir) is one of
; '()
; (cons file lofd)
; (cons dir lofd}

; a file is a string
(define dt1
  (make-dir0 "ts"
            (list "read"
                  (make-dir0 "text"
                            (list "part1"
                                  "part2"
                                  "part3")
                            1
                            "100")
                  (make-dir0 "libs"
                            (list
                             (make-dir0 "code"
                                       (list "hang" "draw")
                                       1 "010")
                             (make-dir0 "docs"
                                       (list "read")
                                       1 "001"))
                            1 "110"))
            1 "011"))
(define (how-many2 dt)
  (cond
    [(string? dt) 1]
    [(empty? (dir0-content dt)) 0]
    [else (apply + (map how-many2 (dir0-content dt)))]))
(check-expect (how-many2 dt1) 7)



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

(define bt5 (make-dir2 "ts" 
                         (list (make-dir2 "d1" '() (list
                                                    (make-file2 "text" 10 "")))
                               (make-dir2 "d2" '() (list
                                                    (make-file2 "text" 10 "")))
                               (make-dir2 "d4" '() (list
                                                    (make-file2 "text" 10 ""))))
                         (list (make-file2 "text" 10 ""))))


(define (how-many3 bt)
  (+
   (length (dir2-files bt))
   (apply + (map how-many3 (dir2-dirs bt)))))

(check-expect (how-many3 bt3) 7)
(define (find1 n dt)
  (cond
    [(member? n (map file2-name (dir2-files dt)))
     (list (dir2-name dt) n)]
    [(not (empty? (dir2-dirs dt)))
     (filter not-false-end? 
             (map (lambda (d)
                    (cons (dir2-name dt)
                          (find1 n d))) (dir2-dirs dt)))]
    [else (list #f)]
    ))

(define (find2 n lod dname)
  (cond
    [(empty? lod) (list #f)]
    [else 
     (empty->false
      (filter not-false-end? 
              (map (lambda (d) (cons dname (find-h n d))) lod)))]))

(define (empty->false lst)
  (if (empty? lst) (list #f) lst))

(define (find-h n d)
  (cond
    [(member? n (map file2-name (dir2-files d))) (list (dir2-name d) n)]
    [else (find2 n (dir2-dirs d) (dir2-name d))]))

(define (find-h2 n d)
  (local
    ((define res (find2 n (dir2-dirs d) (dir2-name d))))
    (if (member? n (map file2-name (dir2-files d))) 
        (cons (list (dir2-name d) n) res)
        res)))

(define (not-false-end? lst)
  (not (false? (last lst))))


(define (ls-R lod dname)
  (cond
    [(empty? lod) '()]
    [else 
     (local ((define res (map ls-h lod)))
       (map (lambda (n) (add-names dname n)) res))]))

(define (ls-h dtree)
  (append
   (map (lambda (n) (list (dir2-name dtree) n)) 
        (map file2-name (dir2-files dtree)))
   (ls-R (dir2-dirs dtree) (dir2-name dtree))))

(define (add-names dname s-or-los)
  (cond
    [(string? s-or-los) (list dname s-or-los)]
    [(list? s-or-los)
     (map (lambda (n) (list dname n)) s-or-los)]))
   
  
                   



(require htdp/dir)

; string -> dir
; creates a representation of the a-path directory
(define W (create-dir
  "C:\\Users\\linhchi.nguyen\\Documents\\htdp\\"))

;(define W (create-dir
;           "R:\\htdp\\"))


(define (how-many4 bt)
  (+
   (length (dir-files bt))
   (apply + (map how-many4 (dir-dirs bt)))))

(define (find? n bt)
  (foldr (lambda (nxt init)
           (or (member? n (map file-name init))
               (member? n (map file-name (dir-files nxt)))))
         (dir-files bt)
         (dir-dirs bt)))

(check-expect (find? "abstract.rkt" W) #t)
(check-expect (find? "index" W) #t)

(define (ls bt)
  (append
   (map file-name (dir-files bt))
   (map dir-name (dir-dirs bt))))

(define (du bt)
  (+ (all-files bt) (depth2 bt)))

(define (all-files bt)
  (apply +
         (append
          (map file-size (dir-files bt))
          (map all-files (dir-dirs bt)))))

(define (depth2* bt)
  (+ (length (dir-dirs bt))
     (apply + (map depth2* (dir-dirs bt)))))
(define (depth2 bt)
  (+ 1 (depth2* bt)))

; a path is [list of string]
; interpretation directions into a dir tree

(define (find n dt)
  (cond
    [(member? n (map file-name (dir-files dt))) (dir-name dt)]
    [else (map (lambda (x) (find n x)) (dir-dirs dt))]))








(test)