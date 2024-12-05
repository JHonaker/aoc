#lang racket

;; Parse Input

(define (parse-file filename)
  (define (parse-line line)
    (match-let* ([(list sym-str num-str) (string-split line " ")]
                 [sym (string->symbol sym-str)]
                 [num (string->number num-str)])
      (make-list num sym)))
  (append-map parse-line (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(struct pt (x y) #:transparent)

(define up (pt 0 1))
(define down (pt 0 -1))
(define left (pt -1 0))
(define right (pt 1 0))

(define (pt+ l r)
  (pt (+ (pt-x l) (pt-x r))
      (+ (pt-y l) (pt-y r))))

(define (pt- l r)
  (pt (- (pt-x l) (pt-x r))
      (- (pt-y l) (pt-y r))))

(define (move-head rope-head direction)
  (match direction
    ['U (pt+ rope-head up)]
    ['D (pt+ rope-head down)]
    ['L (pt+ rope-head left)]
    ['R (pt+ rope-head right)]))

(define (move-tail rope-head rope-tail)
  (match (pt- rope-head rope-tail)
    [(pt 0 2) (pt+ rope-tail up)]
    [(pt 0 -2) (pt+ rope-tail down)]
    [(pt -2 0) (pt+ rope-tail left)]
    [(pt 2 0) (pt+ rope-tail right)]
    [(pt 0 0) rope-tail]
    ;; Head only moves by 1, so each dimension can only differ by 0, 1, or 2
    ;; These two cases covers the orthogonally touching cases
    [(pt _ 0) rope-tail]
    [(pt 0 _) rope-tail]
    ;; This case handles the diagonal and off-diagonal cases
    [(pt x y)
     (if (and (<= -1 x 1)
              (<= -1 y 1))
         rope-tail
         (pt+ rope-tail
              (pt (/ x (abs x))
                  (/ y (abs y)))))]))

(define (move-rope-link rope direction)
  (match-define (cons rope-head rope-tail) rope)
  (let ([new-head (move-head rope-head direction)])
    (cons new-head (move-tail new-head rope-tail))))

(define (part-1 dirs)
  (for/fold ([rope (cons (pt 0 0) (pt 0 0))]
             [tails (list (pt 0 0))]
             #:result (length (remove-duplicates tails)))
            ([dir (in-list dirs)])
    (define moved-rope (move-rope-link rope dir))
    (values moved-rope
            (cons (cdr moved-rope) tails))))

(part-1 ex-input)
(part-1 my-input)

;; Part 2

(define ex-input2 (parse-file "ex-input2"))

;; Now rope is a list of pt
(define long-rope (make-list 10 (pt 0 0)))

(define (move-rope rope direction)
  (let loop ([moved-rope (list (move-head (car rope) direction))]
             [rope (cdr rope)])
    (if (empty? rope)
        (reverse moved-rope)
        (loop (cons (move-tail (car moved-rope) (car rope))
                    moved-rope)
              (cdr rope)))))

(define (part-2 dirs)
  (for/fold ([rope long-rope]
             [tails '()]
             #:result (length (remove-duplicates tails)))
            ([dir (in-list dirs)])
    (define moved-rope (move-rope rope dir))
    (values moved-rope
            (cons (last moved-rope) tails))))

(part-2 ex-input)
(part-2 ex-input2)
(part-2 my-input)
