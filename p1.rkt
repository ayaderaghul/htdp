#lang racket
(require test-engine/racket-tests)
(define-struct spider [legs space] #:transparent)
(define-struct elephant [space] #:transparent)
(define-struct boa-constrictor [length girth]  #:transparent)
(define-struct armadillos [age] #:transparent)

(define-struct cage [w1 w2 h] #:transparent)

(define COMFORT-FACTOR 1.5)

(define (how-much-space animal)
  (* COMFORT-FACTOR
     (cond
       [(spider? animal) (spider-space animal)]
       [(elephant? animal) (elephant-space animal)]
       [(boa-constrictor? animal) (* (boa-constrictor-length animal) 
                                     (boa-constrictor-girth animal))]
       [(armadillos? animal) (* (armadillos-age animal) 5)])))

(check-within (how-much-space (make-spider 5 10)) 15 0.001)
(check-within (how-much-space (make-elephant 100)) 150 0.001)
(check-within (how-much-space (make-boa-constrictor 15 0.5)) (* 15 0.5 1.5) 0.001)
(check-within (how-much-space (make-armadillos 3)) (* 3 5 1.5) 0.001)

(define (fit? animal a-cage)
  (define volume (* (cage-w1 a-cage) (cage-w2 a-cage) (cage-h a-cage)))
  (<= (how-much-space animal) volume))

(check-expect (fit? (make-spider 5 10) (make-cage 5 5 5)) #t)

;;;

(define-struct vehicle [type passengers license fuel] #:transparent)

(define-struct posn [x y] #:transparent)
(define-struct coordinate [neg pos posn] #:transparent)

(make-coordinate -5 5 (make-posn 4 4))




(test)