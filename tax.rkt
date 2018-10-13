#lang racket
(require test-engine/racket-tests)

; a price falls into one of three intervals
; 0 -> 1000
; 1000 -> 10000
; 10000 -> above
; interpretation: price of an item

(define MED 1000)
(define LUX 10000)


; price ->number
; computes the amount of tax charged for p

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p MED)) 0]
    [(and (<= MED p) (< p LUX)) (* 0.05 p)]
    [(>= p LUX) (* 0.08 p)]))
  

(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 1282) (* 0.05 1282))
(check-expect (sales-tax 10000) (* 0.08 10000))
(check-expect (sales-tax 12017) (* 0.08 12017))

