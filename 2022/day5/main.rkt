#lang racket

#|
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

When we read this in, we'll collect lines until we get to the blank, then
dispatch to parse-start-state and parse-instructions.

|#

(define empty-column '())

(define (empty-string? x)
  (zero? (string-length x)))

(define (next-is-blank? x)
  (and (empty-string? (first x))
       (empty-string? (second x))))

(define column-width 3)
(define column-spacing 1)
(define (box-label s i)
  (define index
    (+ (* i (+ column-width
             column-spacing))
       1))
  (cond
    [(< (string-length s) index) #f]
    [(string=? (substring s index (add1 index)) " ") #f]
    [else (string->symbol (substring s index (add1 index)))]))

(define (parse-file filename)
  (let ([lines (file->lines filename)])
    (let loop ([preamble '()]
               [lines lines])
      (if (empty-string? (car lines))
          (append (parse-preamble preamble)
                  (parse-instruction-set (cdr lines)))
          (loop (cons (car lines) preamble)
                (cdr lines))))))

(struct push (location label) #:transparent)
(struct move (from to amount) #:transparent)

#|
parse-start state gets a set of lines like
 1   2   3   ; Column number
[Z] [M] [P]  ; Bottom
[N] [C]      ;
    [D]      ; Top
|#
(define (parse-preamble lines)
  (define num-columns (apply
                       max
                       (filter identity
                               (map string->number
                                    (string-split
                                     (first lines)
                                     " ")))))
  (parse-columns num-columns (rest lines)))

(define (parse-layer-line num-columns line)
  (map (curry box-label line) (range num-columns)))

(define (line->instructions num-columns line)
  (let loop ([line (parse-layer-line num-columns line)]
             [col-num 0]
             [instructions '()])
    (if (empty? line)
        (reverse instructions)
        (let ([label (car line)])
          (loop (cdr line)
                (add1 col-num)
                (if label
                    (cons (push col-num label) instructions)
                    instructions))))))

(define (parse-columns num-columns lines)
  (append-map (curry line->instructions num-columns) lines))

(define (parse-move amount from-col to-col)
  (move (sub1 from-col) (sub1 to-col) amount))

(define (parse-line line)
  (match (string-split line " ")
    [(list "move" amount "from" from "to" to)
     (parse-move (string->number amount)
                 (string->number from)
                 (string->number to))]))

(define (parse-instruction-set lines)
  (map parse-line lines))

(define (interpret-push state column label)
  (list-update state column (lambda (xs) (cons label xs))))

(define (interpret-move-1 state from to amount)
  (define old-from (list-ref state from))
  (define new-from (drop old-from amount))
  (define moving-blocks (reverse (take old-from amount)))
  (define new-to (append moving-blocks (list-ref state to)))
  (list-set (list-set state to new-to) from new-from))

(define (interpret-command-1 state instruction)
  (match instruction
    [(push column label)
     (interpret-push state column label)]
    [(move from to amt)
     (interpret-move-1 state from to amt)]))

(define (make-blank-state k)
  (make-list k empty-column))

(define (interpreter interpret-command instructions)
  (let loop ([state (make-blank-state 10)]
             [instructions instructions])
    (if (empty? instructions)
        state
        (loop (interpret-command state (car instructions))
              (cdr instructions)))))

(define (interpreter-1 instructions)
  (interpreter interpret-command-1 instructions))

;; Part 2

(define (interpret-move-2 state from to amount)
  (define old-from (list-ref state from))
  (define new-from (drop old-from amount))
  (define moving-blocks (take old-from amount))
  (define new-to (append moving-blocks (list-ref state to)))
  (list-set (list-set state to new-to) from new-from))

(define (interpret-command-2 state instruction)
  (match instruction
    [(push column label)
     (interpret-push state column label)]
    [(move from to amt)
     (interpret-move-2 state from to amt)]))

(define (interpreter-2 instructions)
  (interpreter interpret-command-2 instructions))

(define ex-input (parse-file "ex-input"))
(interpreter-1 ex-input)
(interpreter-2 ex-input)

(define my-input (parse-file "my-input"))
(interpreter-1 my-input)
(interpreter-2 my-input)
