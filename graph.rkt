#lang racket
(require test-engine/racket-tests)

; a node is a symbol

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define (neighbours n g)
  (local ((define res (assq n g)))
    (if res (rest res) #f)))

(check-expect (neighbours 'A sample-graph) '(B E))
(check-expect (neighbours 'H sample-graph) #f)
          
    

; a path is a [list of node]
; interpretation the list of nodes specifies a sequence
; of immediate neighbors that leads from the first
; node on the list to the last one

; node node graph -> [maybe path]
; finds a path from origination to destination in G
(define (find-path origin destination G)
  (cond
    [(symbol=? origin destination) (list destination)]
    [else (local ((define next (neighbours origin G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #f]
              [else (cons origin candidate)]))]))

; [list of node] node graph -> [maybe path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #f

(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #f]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))


(check-expect (find-path 'C 'D sample-graph) '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #f)

(define (find-path/list2 loo d g)
  (ormap (lambda (x) (find-path x d g)) loo))

(define (neighbours2 o g points-traversed)
  (local ((define res (neighbours o g))
          (define m (how-many? o points-traversed)))
    (list (cons o points-traversed)
          (drop res m))))

(define (how-many? x lst)
  (length
   (filter true? 
           (map (lambda (e) (equal? x e)) lst))))

(define (true? x) (not (false? x)))



;;; HOW TO GET RID OF CYCLE IN CYCLIC GRAPH????
(define (find-path2 origin destination G points-traversed)
  (cond
    [(symbol=? origin destination) (list destination)]
    [else (local ((define next (neighbours2 origin G points-traversed))
                  (define candidate
                    (ormap 
                     (lambda (x) 
                       (find-path2 x destination G (first next))) 
                     (last next))))
            (cond
              [(boolean? candidate) #f]
              [else (cons origin candidate)]))]))

;; not the shortest path but at least it fucking works
(check-expect (find-path2 'A 'D g1 '())
              '(A B E C B F D))
           

(define (test-on-all-nodes g)
  (local ((define nodes (map first g))
          (define (combinations lon)
            (cond
              [(empty? lon) '()]
              [else (append
                     (for/list ([i (rest lon)])
                       (list (first lon) i))
                     (combinations (rest lon)))]))
          (define comb (combinations nodes)))
    (andmap (lambda (x) (find-path (first x) (second x) g)) comb))) 
              
(define g1
  '((A B E)
    (B E F)
    (C B D)
    (D)
    (E C F)
    (F D G)
    (G)))


(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; an fsm is a structure:
; (make-fsm fsm-state [list of 1transition] fsm-state)
; a 1transition is a structure:
; (make-transition fsm-state 1string fsm-state)
; an fsm-state is string
; data expmale:

(define fsm-ab-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; fsm string -> boolean
; does an-fsm recongnize the given string
(define (fsm-match? an-fsm a-string)
 #f)

(define (explode str)
  (map list->string (map list (string->list str))))


(test)