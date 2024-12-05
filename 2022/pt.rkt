#lang racket

(provide (all-defined-out))

(define (between? x y val)
  (or (<= x val y)
      (<= y val x)))

(struct pt (x y) #:transparent)

(define (pt+ l r)
  (match-let ([(pt l-x l-y) l]
              [(pt r-x r-y) r])
    (pt (+ l-x r-x)
        (+ l-y r-y))))

(define (pt- l r)
  (match-let ([(pt l-x l-y) l]
              [(pt r-x r-y) r])
    (pt (- l-x r-x)
        (- l-y r-y))))

(define (pt-same-x? . pts)
  (apply = (map pt-x pts)))

(define (pt-same-y? . pts)
  (apply = (map pt-y pts)))

(define (pt-between? side-1 side-2 test-pt)
  (or (and (pt-same-x? side-1 side-2 test-pt)
           (between? (pt-y side-1) (pt-y side-2) (pt-y test-pt)))
      (and (pt-same-y? side-1 side-2 test-pt)
           (between? (pt-x side-1) (pt-x side-2) (pt-x test-pt)))))
