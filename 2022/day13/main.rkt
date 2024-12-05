#lang racket

;; Parse input

(struct pair (left right) #:transparent)

;; Grammar
;; Expr :: ( Expr (, Expr)* ) | Num

(define (parse-packet xs)
  (let loop ([xs xs]
             [stack '()])
    (match xs
      ['() (values (reverse stack) '())]
      [(cons #\[ rest-xs)
       (define-values (packet rem-xs) (parse-packet rest-xs))
       (loop rem-xs (cons packet stack))]
      [(cons #\] rest-xs) (values (reverse stack) rest-xs)]
      [(cons #\, rest-xs)
       (define-values (packet rem-xs) (parse-packet rest-xs))
       (loop rem-xs (cons packet stack))]
      [xs (parse-number xs)])))

(define (parse-number xs)
  (define (convert num-list)
    (string->number (apply string (reverse num-list))))
  (let loop ([xs xs]
             [num-list '()])
    (cond
      [(empty? xs)
       (values (convert num-list) xs)]
      [(char-numeric? (car xs))
       (loop (cdr xs)
             (cons (car xs) num-list))]
      [else
       (values (convert num-list) xs)])))

(define (parse-pair two-lines)
  (define (process line)
    (define-values (packet _) (parse-packet (string->list line)))
    packet)
  (pair (process (first two-lines))
        (process (second two-lines))))

(define (parse-lines lines)
  (cond
    [(empty? lines) '()]
    [(string=? (car lines) "") (parse-lines (cdr lines))]
    [else (cons (parse-pair (take lines 2))
                (parse-lines (drop lines 2)))]))

(define (parse-file filename)
  (parse-lines (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(define (numbers<? left right)
    (cond
      [(< left right) #t]
      [(> left right) #f]
      [else 'unknown]))

(define (list<? left right)
  (cond
    [(and (empty? left) (empty? right)) 'unknown]
    [(empty? left) #t]
    [(empty? right) #f]
    [else
     (match (packet<? (car left) (car right))
       ['unknown (list<? (cdr left) (cdr right))]
       [ord ord])]))

(define (packet<? left right)
  (cond
    [(and (number? left) (number? right))
     (numbers<? left right)]
    [(and (list? left) (list? right))
     (list<? left right)]
    [(list? left)
     (list<? left (list right))]
    [(list? right)
     (list<? (list left) right)]))

(define (packet-pair<? ps)
  (packet<? (pair-left ps) (pair-right ps)))

(define (in-order-indices input)
  (filter-map (λ (b i) (if b i #f))
            (map packet-pair<? input)
            (inclusive-range 1 (length input))))

(apply + (in-order-indices ex-input))
(apply + (in-order-indices my-input))

;; Part 2

(define (flatten-pairs pairs)
  (append-map (λ (p) (list (pair-left p) (pair-right p))) pairs))

(define (decoder-key pairs)
  (define flattened (flatten-pairs pairs))
  (define to-sort (append flattened (list '((2)) '((6)))))
  (define sorted (sort to-sort packet<?))
  (define two-key (add1 (index-of sorted '((2)))))
  (define six-key (add1 (index-of sorted '((6)))))
  (* two-key six-key))

(decoder-key ex-input)
(decoder-key my-input)
