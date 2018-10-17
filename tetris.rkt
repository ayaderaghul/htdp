#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define W 10) ; # of blocks, horizontally 
(define S 10) ; blocks are squares
(define SCENE-SIZE (* W S))
(define B (empty-scene SCENE-SIZE SCENE-SIZE))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- S 1) "solid" "red")
    (square S "outline" "black")))

(define-struct tetris [block landscape] #:transparent)
(define-struct block [x y] #:transparent)

(define (new-block)
  (make-block (* (random W) S) 0))

(define B0 (new-block))
(define T0 (make-tetris B0 '()))
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting


(define (render tet)
  (define block (tetris-block tet))
  (define landscape (tetris-landscape tet))
  (place-blocks (cons block landscape)))

(define (place-blocks lob)
  (cond
    [(empty? (rest lob)) (place-block (first lob) B)]
    [else (place-block (first lob)
                       (place-blocks (rest lob)))])) 
           
(define (place-block bl im)
  (place-image BLOCK (block-x bl) (block-y bl) im))


(define (drop tet)
  (define block (tetris-block tet))
  (define y (block-y block))
  (define land (tetris-landscape tet))
  (define new-block
    (make-block (block-x block) (+ y S)))
  (make-tetris new-block land))
  
(define (move tet ke)
  (define block (tetris-block tet))
  (define x (block-x block))
  (define y (block-y block))
  (define land (tetris-landscape tet))
  
  (define new-left (make-block (- x S) y))
  (define new-right (make-block (+ x S) y))
  
  (define new-block
    (make-block 
     (cond 
       [(string=? ke "left") (if (land? new-left land) x (- x S))]
       [(string=? ke "right") (if (land? new-right land) x (+ x 10))]
       [else x])
     y))
  (make-tetris new-block land))
               
(define (new-tetris block lands)
  ;(define new-y (- (block-y block) S))
  (make-tetris (new-block) (cons block lands)))

(define (tock tet)
  (define block (tetris-block tet))
  (define lands (tetris-landscape tet))
  (if (land? block lands)
      (new-tetris block lands)
      (drop tet)))

(define (land? block lands)
  (define x (block-x block))
  (define y (block-y block))
  (define y-min (ground-height (block-uppers x lands)))
  (>= (+ y (/ S 2)) y-min))

(define (block-uppers x lands)
  (cond
    [(empty? lands) (list SCENE-SIZE)]
    [else (if (= x (block-x (first lands))) 
              (cons (block-y (first lands)) (block-uppers x (rest lands)))
              (block-uppers x (rest lands)))]))

(define (ground-height lst)
  (- (apply min lst) (/ S 2)))
                 
(define T1
  (make-tetris (make-block 20 10) (list (make-block 20 90) (make-block 30 90))))
  
(define (all-ground-heights x lands)
  (cond
    [(zero? x) (list (ground-height (block-uppers x lands)))]
    [else (cons (ground-height (block-uppers x lands))
                (all-ground-heights (- x S) lands))]))
(define (peak lands)
  (ground-height (all-ground-heights (* (sub1 W) S) lands)))
                     
(define (over? tet)
  (define lands (tetris-landscape tet))
  (define p (peak lands))
  (<= p 0))


(define T3 (make-tetris (make-block 40 0) 
                        (list (make-block 40 90)
                              (make-block 40 80)
                              (make-block 40 70)
                              (make-block 40 60)
                              (make-block 40 50)
                              (make-block 40 40)
                              (make-block 40 30)
                              (make-block 40 20)
                              (make-block 40 10))))
                              
(check-expect (over? T3) #t)
(define (create-tl tl)
  tl)

(define (main r)
  (define tet0 (make-tetris (new-block) '()))
  (big-bang tet0 
            [on-tick tock r]
            [on-key move]
            [to-draw render]
            [state #t]
            [stop-when over?]))


(test)