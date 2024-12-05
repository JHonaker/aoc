#lang racket

(require "../pt.rkt")

(define pt-down (pt 0 1))
(define pt-up (pt 0 -1))
(define pt-right (pt 1 0))
(define pt-left (pt -1 0))
(define pt-downleft (pt+ pt-down pt-left))
(define pt-downright (pt+ pt-down pt-right))

;; Data types

;; Segment: (struct Pt Pt)
;; Line: Listof Segment
;; Cave: Hashof Int Column
;; Column: Listof Int

;; A segment is a struct with two `pt' elements that define the start and end
;; locations.
(struct segment (start end) #:transparent)

(define (in-segment? seg point)
  (pt-between? (segment-start seg)
               (segment-end seg)
               point))

(define (horizontal? seg)
  (match-define (segment start end) seg)
  (= (pt-y start) (pt-y end)))

(define (vertical? seg)
  (match-define (segment start end) seg)
  (= (pt-x start) (pt-x end)))

(define (x-values seg)
  (define start-x (pt-x (segment-start seg)))
  (define end-x (pt-x (segment-end seg)))
  (inclusive-range (min start-x end-x)
                   (max start-x end-x)))

(define (y-values seg)
  (define start-y (pt-y (segment-start seg)))
  (define end-y (pt-y (segment-end seg)))
  (inclusive-range (min start-y end-y)
                   (max start-y end-y)))

;; A line is a list of `segment' structs where consecutive segments in the list
;; have coinciding start and end points
;; make-line: Listof Pt -> Line
(define (make-line points)
  (when (<= (length points) 1)
    (error "Cannot make a line with fewer than two points"))
  (let loop ([points points]
             [segments '()])
    (if (empty? (cdr points))
        (reverse segments)
        (loop (cdr points)
              (cons (segment (first points)
                             (second points))
                    segments)))))

;; Insert into a list without duplicating values
(define (column-insert column value [start-index 0])
  (define head (take column start-index))
  (define tail
    (let loop ([column (drop column start-index)])
      (cond
        [(empty? column)
         (list value)]
        [(< (car column) value)
         (cons (car column)
               (loop (cdr column)))]
        [(= (car column) value)
         column]
        [else
         (cons value column)])))
  (if (empty? head)
      tail
      (append head tail)))

;; A Cave is a (Hash Int (Listof Int))
;; where the key is an x value and the result is a list of y values that are
;; blocked
(define (make-cave lines)
  (define cave (make-hash))
  (define (add-horizontal-segment! seg)
    (define y (pt-y (segment-start seg)))
    (let loop ([xs (x-values seg)])
      (unless (empty? xs)
        (define x (car xs))
        (hash-set! cave x (column-insert (cave-ref cave x) y))
        (loop (cdr xs)))))
  (define (add-vertical-segment! seg)
    (define x (pt-x (segment-start seg)))
    (let loop ([column (cave-ref cave x)]
               [ys (y-values seg)])
      (if (empty? ys)
          (hash-set! cave x column)
          (loop (column-insert column (car ys))
                (cdr ys)))))
  (for* ([line (in-list lines)]
         [seg (in-list line)])
    (if (horizontal? seg)
        (add-horizontal-segment! seg)
        (add-vertical-segment! seg)))
  (make-immutable-hash
   (hash->list cave)))

(define (cave-ref cave x)
  (hash-ref cave x '()))

(define (cave-max-y cave)
  (apply max (map (λ (xs) (apply max xs)) (hash-values cave))))

(define (cave-min-x cave)
  (apply min (hash-keys cave)))

(define (cave-max-x cave)
  (apply max (hash-keys cave)))

;; A unit of sand is represented by a segment that has the same start and end
;; point.
;; add-sand: Cave Int Int -> Cave
(define (add-sand/xy cave x y)
  (hash-set cave x
             (column-insert (cave-ref cave x) y)))

(define (add-sand cave point)
  (add-sand/xy cave (pt-x point) (pt-y point)))

(define (display-cave start-cave cave)
  (define max-y (cave-max-y cave))
  (define min-x (cave-min-x cave))
  (define max-x (cave-max-x cave))
  (for ([y (in-inclusive-range 0 max-y)])
    (for ([x (in-inclusive-range min-x max-x)])
      (cond
        [(and (= x 500) (= y 0))
         (display #\+)]
        [(blocked? start-cave (pt x y))
         (display #\#)]
        [(blocked? cave (pt x y))
         (display #\o)]
        [else
         [display #\.]]))
    (newline)))

;; Parsing

(define (parse-file filename)
  (let ([lines (file->lines filename)])
    (make-cave (map parse-line lines))))

(define (parse-line line)
  (make-line (map parse-point (string-split line " -> "))))

(define (parse-point point)
  (match-define (list x-str y-str) (string-split point ","))
  (pt (string->number x-str)
      (string->number y-str)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

;; A piece of sand falls forever if there is no blocking y-value under it
(define (falls-forever? cave sand)
  (match-define (pt x y) sand)
  (or (false? y)
      (empty?
       (dropf (cave-ref cave x)
              (λ (col-y) (< col-y y))))))

(define (highest-solid-y cave x #:below [below 0])
  (define column (dropf (cave-ref cave x)
                        (λ (col-y) (< col-y below))))
  (if (empty? column)
      #f
      (car column)))

(define (find-steady-state cave)
  (define source (pt 500 0))

  (define (drop-to-top source)
    (define sand (pt (pt-x source)
                     (let ([y (highest-solid-y cave
                                               (pt-x source)
                                               #:below (pt-y source))])
                       (if y (sub1 y) #f))))
    (if (falls-forever? cave sand)
        cave
        (let ([blocked-downleft (blocked? cave (pt+ sand pt-downleft))]
              [blocked-downright (blocked? cave (pt+ sand pt-downright))])
          (cond
            [(not blocked-downleft)
             (drop-to-top (pt+ sand pt-downleft))]
            [(not blocked-downright)
             (drop-to-top (pt+ sand pt-downright))]
            [else
             (find-steady-state (add-sand cave sand))]))))

  (if (blocked? cave source)
      cave
      (drop-to-top source)))

(define (blocked? cave point)
  (let loop ([column (cave-ref cave (pt-x point))])
    (if (empty? column)
        #f
        (or (= (car column) (pt-y point))
            (loop (cdr column))))))


(define (count-cells cave)
  (for/fold ([total 0])
              ([(x col) (in-hash cave)])
      (+ total (length col))))

(define (sand-added start end)
  ;; The raw count doesn't deal well with the expanding, inifintely wide bottom
  ;; row. We need to subtract the count of the bottom row because it will always
  ;; be blocks, but maybe a different width
  (define (corrected-count cave)
    (define min-x (apply min (hash-keys cave)))
    (define max-x (apply max (hash-keys cave)))

    (define uncorrected (count-cells cave))
    (- uncorrected (- max-x min-x)))

  (- (corrected-count end)
     (corrected-count start)))

(define ss-ex-1 (find-steady-state ex-input))
(define ss-my-1 (find-steady-state my-input))

(sand-added ex-input ss-ex-1)
(sand-added my-input ss-my-1)

;; Part 2

(define (add-floor cave)
  (define cave-floor (+ (cave-max-y cave) 2))
  (for/hash ([(x col) (in-hash cave)])
    (values x (append col (list cave-floor)))))

(define ex-floor (+ (cave-max-y ex-input) 2))
(define my-floor (+ (cave-max-y my-input) 2))

(define ex-input-2 (add-floor ex-input))
(define my-input-2 (add-floor my-input))

(set! cave-ref
  (lambda (cave x)
    (hash-ref cave x (list ex-floor))))

(define ss-ex-2 (find-steady-state ex-input-2))

(sand-added ex-input-2 ss-ex-2)

(set! cave-ref
  (lambda (cave x)
    (hash-ref cave x (list my-floor))))

(define ss-my-2 (find-steady-state my-input-2))

(sand-added my-input-2 ss-my-2)
