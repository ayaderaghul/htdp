#lang racket

(require 2htdp/image 2htdp/universe test-engine/racket-tests)

(define-struct game [plane water fires] #:transparent)
(define-struct plane [loc vel] #:transparent)

(define BLOCK-SIZE 10)
(define BLOCKS 20)
(define SCENE-SIZE (* BLOCK-SIZE BLOCKS))
(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))

(define PLANE-HEIGHT 10)
(define PLANE (rectangle 20 PLANE-HEIGHT "solid" "green"))

(define WATER (square BLOCK-SIZE "solid" "blue"))
(define FIRE (square BLOCK-SIZE "solid" "red"))

(define WATER-V 5)
(define FIRE-V 1)

(define g1 (make-game (make-plane 20 10) '() '()))

(define (render-fires lof)
  (cond
    [(empty? lof) BACKGROUND]
    [else (place-image FIRE (first (first lof))
                       (second (first lof))
                       (render-fires (rest lof)))]))

(define (render-water w im)
  (if (empty? w) im
      (place-image WATER (first w) (second w) im)))

(define (render-plane p im)
  (place-image PLANE (plane-loc p) SCENE-SIZE im))

(define (render g)
  (render-plane (game-plane g)
                (render-water (game-water g)
                              (render-fires (game-fires g)))))

(define (change g ke)
  (make-game
   (cond
     [(or (string=? ke "left")
          (string=? ke "right"))
      (control-plane (game-plane g) ke)]
     [else (game-plane g)])
   (cond
     [(string=? ke " ")
      (append (list (plane-loc (game-plane g)) (- SCENE-SIZE 2))
              (game-water g))]
     [else (game-water g)])
   (game-fires g)))

(define (control-plane p ke)
  (define v (plane-vel p))
  (make-plane (plane-loc p)
              (cond
                [(string=? ke "left") (- (abs v))]
                [(string=? ke "right") (abs v)]
                [else v])))
              
  

(define (tock g)
  (make-game
   (move-plane (game-plane g))
   (move-water (game-water g))
   (set-fires (game-fires g))))

(define (move-plane p)
  (define v (plane-vel p))
  (define v+ (+ (plane-loc p) v))
  (make-plane 
   (bounded v+ 0 SCENE-SIZE) v))

(define (bounded n a b)
  (cond
    [(< n a) a]
    [(> n b) b]
    [else n]))

(define (move-water w)
  (if (empty? w) '()
      (list (first w) (- (second w) BLOCK-SIZE))))

(define (random-member lst)
  (define l (length lst))
  (define r (random l))
  (list-ref lst r))

(define (set-fires f)
  (cond
    [(empty? f) (list (list (* (random BLOCKS) BLOCK-SIZE) 0))]
    [else
     (cons
      (random-member
       (possible-fire
        (random-member (fire-line (- SCENE-SIZE BLOCK-SIZE) f))
        f))
      f)]))
    
(define (member? n lst)
  (cond
    [(empty? lst) #f]
    [else (if (equal? n (first lst))
              #t
              (member? n (rest lst)))]))

(define (possible-fire block f)
  (match-define (list x y) block)
  (define b+ 
    (cond
      [(>= x (- SCENE-SIZE 10)) '()]
      [(member? (list (+ x 10) y) f) '()]
      [else (list (list (+ x 10) y))]))
  
  (define b-
    (cond
      [(<= x 0) '()]
      [(member? (list (- x 10) y) f) '()]
      [else (list (list (- x 10) y))]))
   
  (define b> (list (list x (+ y 10))))
  (append b+ b- b>))

(define (fires-at x f)
  (cond
    [(empty? f) (list (list x 0))]
    [else (if (= x (first (first f)))
              (cons (first f) (fires-at x (rest f)))
              (fires-at x (rest f)))]))

(check-expect (fires-at 10 (list (list 10 20) (list 10 0) (list 0 30)))
              (list (list 10 20) (list 10 0)))

(define (peak lst)
  (first (sort< lst)))
(define (sort< lst)
  (cond
    [(empty? lst) '()]
    [else (insert (first lst) (sort< (rest lst)))]))
(define (insert pr lst)
  (cond
    [(empty? lst) (list pr)]
    [else
     (if (<= (second pr) (second (first lst))) 
         (cons pr lst) 
         (cons (first lst) (insert pr (rest lst))))]))

(define (fire-line x f)
  (cond
    [(zero? x) (list (peak (fires-at x f)))]
    [else (cons (peak (fires-at x f))
                (fire-line (- x BLOCK-SIZE) f))]))

(define (main g0)
  (big-bang g0
            [on-tick tock 0.3 20]
            [on-key change]
            [to-draw render]))