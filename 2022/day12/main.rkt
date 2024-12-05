#lang racket

;; Starting position has height 'a'
;; End position has height 'z'

;; Destination square height <= Current height + 1

(define (parse-file filename)
  (list->vector
   (map list->vector
        (map string->list (file->lines filename)))))

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

(define pt-right (pt 1 0))
(define pt-left (pt -1 0))
(define pt-up (pt 0 1))
(define pt-down (pt 0 -1))

(define pt-dirs (list pt-right pt-left pt-up pt-down))

(define (grid-ref grid loc)
  (match-define (pt x y) loc)
  (vector-ref (vector-ref grid y) x))

(define (cost-fn grid loc)
  (define a (char->integer #\a))
  (match (grid-ref grid loc)
    [#\S 0]
    [#\E 25]
    [c (- (char->integer c) a)]))

(define (neighbors loc)
  (map (curry pt+ loc) pt-dirs))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))



;; Part 1

(define (bfs start grid valid-move? at-goal?)
  (define visited (mutable-set start))
  (let loop ([queue (list (cons start 0))])
    (cond
      [(empty? queue) #f]
      [else
       (match-define (cons current-location current-cost) (car queue))
       (if (at-goal? current-location)
           current-cost
           (let* ([nbrs (neighbors current-location)]
                  [not-visited (filter (λ (next)
                                         (not (set-member? visited next)))
                                       nbrs)]
                  [valid-moves (filter (λ (next)
                                         (valid-move? grid current-location next))
                                       not-visited)]
                  [new-moves (map (λ (next)
                                    (set-add! visited next)
                                    (cons next (add1 current-cost)))
                                  valid-moves)]
                  [queue (append (cdr queue)
                                 new-moves)])
             (loop queue)))])))

(define (in-bounds? loc H W)
  (match-define (pt x y) loc)
  (and (<= 0 x)
       (< x W)
       (<= 0 y)
       (< y H)))

(define (can-step-up? grid current next)
  (define current-height (cost-fn grid current))
  (define next-height (cost-fn grid next))
  (<= next-height (+ current-height 1)))


(define (find-char grid c)
  (first (filter
   identity
   (for*/list ([y (in-range (vector-length grid))]
              [x (in-range (vector-length (vector-ref grid 0)))])
     (if (char=? (grid-ref grid (pt x y)) c)
         (pt x y)
         #f)))))

(define ex-start (find-char ex-input #\S))
(define my-start (find-char my-input #\S))

(define ex-goal (find-char ex-input #\E))
(define my-goal (find-char my-input #\E))

(define ex-H (vector-length ex-input))
(define ex-W (vector-length (vector-ref ex-input 0)))

(define my-H (vector-length my-input))
(define my-W (vector-length (vector-ref my-input 0)))

(bfs ex-start ex-input
     (lambda (grid current next)
       (and (in-bounds? next ex-H ex-W)
            (can-step-up? grid current next)))
     (lambda (current)
       (equal? ex-goal current)))

(bfs my-start my-input
     (lambda (grid current next)
       (and (in-bounds? next my-H my-W)
  (can-step-up? grid current next)))
     (lambda (current)
       (equal? my-goal current)))

;; Part 2

(define ((at-char? grid c) loc)
  (char=? (grid-ref grid loc) c))

(define (backward-can-step-up? grid current next)
  (can-step-up? grid next current))

(bfs ex-goal ex-input
     (lambda (grid current next)
       (and (in-bounds? next ex-H ex-W)
            (backward-can-step-up? grid current next)))
     (at-char? ex-input #\a))

(bfs my-goal my-input
     (lambda (grid current next)
       (and (in-bounds? next my-H my-W)
            (backward-can-step-up? grid current next)))
     (at-char? my-input #\a))
