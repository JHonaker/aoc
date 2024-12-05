#lang racket

;; Setup

(struct monkey (items operation test true-target false-target))

(define ((divisible-by? n) x)
  (zero? (modulo x n)))

(define ex-monkeys
  (vector
   (monkey (list 79 98)
           (λ (old) (* old 19))
           23
           2
           3)
   (monkey (list 54 65 75 74)
           (λ (old) (+ old 6))
           19
           2
           0)
   (monkey (list 79 60 97)
           (λ (old) (* old old))
           13
           1
           3)
   (monkey (list 74)
           (λ (old) (+ old 3))
           17
           0 1)))

(define my-monkeys
  (vector
   (monkey (list 97 81 57 57 91 61)
           (λ (old) (* old 7))
           11
           5
           6)
   (monkey (list 88 62 68 90)
           (λ (old) (* old 17))
           19
           4
           2)
   (monkey (list 74 87)
           (λ (old) (+ old 2))
           5
           7
           4)
   (monkey (list 53 81 60 87 90 99 75)
           (λ (old) (+ old 1))
           2
           2
           1)
   (monkey (list 57)
           (λ (old) (+ old 6))
           13
           7
           0)
   (monkey (list 54 84 91 55 59 72 75 70)
           (λ (old) (* old old))
           7
           6
           3)
   (monkey (list 95 79 79 68 78)
           (λ (old) (+ old 3))
           3
           1
           3)
   (monkey (list 61 97 67)
           (λ (old) (+ old 4))
           17
           0
           5)))

(define (monkey-ref v i)
  (vector-ref v i))

(define (clear-items! monkeys index)
  (vector-set! monkeys index
               (struct-copy monkey (vector-ref monkeys index)
                            [items '()])))

(define (add-items! monkeys items index)
  (define orig-monkey (vector-ref monkeys index))
  (define new-monkey
    (struct-copy monkey orig-monkey
                 [items (append (monkey-items orig-monkey) items)]))
  (vector-set! monkeys index new-monkey))

(define (display-state monkeys)
  (newline)
  (define (display-monkey i)
    (printf "Monkey ~a: ~a~%" i (monkey-items (vector-ref monkeys i))))
  (for ([i (in-range (vector-length monkeys))])
    (display-monkey i)))

;; Part 1

(define (part-1-post item)
  (quotient item 3))

(define counter (make-hash))
(define (clear-counter!) (set! counter (make-hash)))
(define (display-counter) (displayln counter))

(define (count-wrapper f i)
  (lambda xs
    (hash-set! counter i (add1 (hash-ref counter i 0)))
    (apply f xs)))

(define (process-turn! monkeys index relief-op)
  (match-define
    (monkey items op div-number to-true to-false)
    (monkey-ref monkeys index))
  (define worry-levels
    (map (λ (x) (relief-op x))
         (map (count-wrapper op index) items)))
  (define-values (true-group false-group)
    (partition (divisible-by? div-number) worry-levels))
  (clear-items! monkeys index)
  (add-items! monkeys true-group to-true)
  (add-items! monkeys false-group to-false)
  monkeys)

(define (process-round! monkeys relief-op [display? #t])
  (for ([i (in-range (vector-length monkeys))])
    (process-turn! monkeys i relief-op))
  (when display?
    (display-state monkeys)
    (display-counter)))

(define (process-rounds! monkeys n relief-op [display? #t])
  (for ([_ (in-range n)])
    (process-round! monkeys relief-op display?)))

;; Part 1

(define (copy-monkeys monkeys)
  (vector-map (λ (m) (struct-copy monkey m)) monkeys))

(define ex-monkeys-1 (copy-monkeys ex-monkeys))
(define my-monkeys-1 (copy-monkeys my-monkeys))

(process-rounds! ex-monkeys-1 20 part-1-post #f)
(sort (hash->list counter) < #:key cdr)

(clear-counter!)
(process-rounds! my-monkeys-1 20 part-1-post #f)
(sort (hash->list counter) < #:key cdr)

;; Part 2

(define ex-primes (apply lcm (vector->list
                   (vector-map monkey-test ex-monkeys))))

(define my-primes  (apply lcm (vector->list
                    (vector-map monkey-test my-monkeys))))

(define the-prime (* ex-primes my-primes))

(define ex-monkeys-2 (copy-monkeys ex-monkeys))
(define my-monkeys-2 (copy-monkeys my-monkeys))

(define (part-2-post item)
  (modulo item the-prime))

(newline)
(clear-counter!)
(process-rounds! ex-monkeys-2 10000 part-2-post #f)
(sort (hash->list counter) < #:key cdr)


(newline)
(clear-counter!)
(process-rounds! my-monkeys-2 10000 part-2-post #f)
(sort (hash->list counter) < #:key cdr)
