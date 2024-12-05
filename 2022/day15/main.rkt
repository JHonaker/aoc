#lang racket

(require "../pt.rkt")

(module+ test
  (require rackunit))

(struct sensor (loc closest-beacon) #:transparent)

(define (sensor-x the-sensor)
  (pt-x (sensor-loc the-sensor)))

(define (sensor-y the-sensor)
  (pt-y (sensor-loc the-sensor)))

;; (struct Int Int)
(struct interval (start end) #:transparent)
;; A Cover is a (Listof Interval)


(define (make-interval a b)
  (interval (min a b) (max a b)))

(define (interval-length ival)
  (+ (- (interval-end ival)
        (interval-start ival))
     1))

(define (interval<? a b)
  (<= (interval-start a) (interval-start b)))


(define (intervals-overlap? a b)
  (define-values (left right)
    (if (interval<? a b)
        (values a b)
        (values b a)))
  (<= (interval-start right) (interval-end left)))

(module+ test
  (define i1 (interval -1 0))
  (define i2 (interval 0 3))
  (define i3 (interval 2 4))
  (define i4 (interval 1 2))

  (check-true (intervals-overlap? i1 i2))
  (check-true (intervals-overlap? i2 i1))
  (check-true (intervals-overlap? i2 i3))
  (check-true (intervals-overlap? i3 i2))
  (check-false (intervals-overlap? i1 i3))
  (check-false (intervals-overlap? i3 i1))
  (check-true (intervals-overlap? i2 i4)))

(define (interval-contains? container contained #:strict [strict #f])
  (define cmp (if strict < <=))
  (and (cmp (interval-start container)
           (interval-start contained))
       (cmp (interval-end contained)
           (interval-end container))))

(define (merge/intervals a b)
  (if (intervals-overlap? a b)
      (list (interval (min (interval-start a)
                           (interval-start b))
                      (max (interval-end a)
                           (interval-end b))))
      (if (interval<? a b)
          (list a b)
          (list b a))))

(module+ test
  (check-equal? (merge/intervals (interval 0 1) (interval 2 3))
                (list (interval 0 1) (interval 2 3)))
  (check-equal? (merge/intervals (interval 0 1) (interval 1 3))
                (list (interval 0 3)))
  (check-equal? (merge/intervals (interval 0 2) (interval 1 3))
                (list (interval 0 3))))

(define (merge/head ivals)
  (let loop ([head (car ivals)] [tail (cdr ivals)])
    (cond
      [(list? head)
       (if (= (length head) 1)
           (loop (car head) tail)
           (append head tail))]
      [(empty? tail) (list head)]
      [(intervals-overlap? head (car tail))
       (loop (merge/intervals head (car tail))
             (cdr tail))]
      [else (cons head tail)])))

(module+ test
  (check-equal? (merge/head (list (interval 0 1) (interval 1 2)))
                (list (interval 0 2)))
  (check-equal? (merge/head (list (interval 0 1) (interval 2 3)))
                (list (interval 0 1) (interval 2 3)))
  (check-equal? (merge/head (list (interval 0 5) (interval 1 2) (interval 4 7)))
                (list (interval 0 7))))


(define (merge/lists a b)
  (let loop ([a a] [b b])
    (cond
      [(and (empty? a) (empty? b)) '()]
      [(empty? a) b]
      [(empty? b) a]
      [(intervals-overlap? (car a) (car b))
       (define merged (car (merge/intervals (car a) (car b))))
       (loop (merge/head (cons merged (cdr a))) (cdr b))]
      [else
       (if (interval<? (car a) (car b))
           (cons (car a) (loop (cdr a) b))
           (cons (car b) (loop a (cdr b))))])))

(module+ test
  (check-equal? (merge/lists (list (interval 0 1) (interval 3 4))
                             (list (interval 1 3)))
                (list (interval 0 4))))


(define (merge a b)
  (cond
    [(and (interval? a) (interval? b))
     (merge/intervals a b)]
    [(and (interval? a) (list? b))
     (merge/lists (list a) b)]
    [(and (list? a) (interval? b))
     (merge/lists a (list b))]
    [else
     (merge/lists a b)]))

(define (trim-interval ival
                       #:from-start [start-trim 0]
                       #:from-end [end-trim 0])
  (make-interval (+ (interval-start ival) start-trim)
                 (- (interval-end ival) end-trim)))

(define (cut-interval ival value)
  (cond
    [(= (interval-start ival) value)
     (list (trim-interval ival #:from-start 1))]
    [(= (interval-end ival) value)
     (list (trim-interval ival #:from-end 1))]
    [(< (interval-start ival) value (interval-end ival))
     (list (interval (interval-start ival)
                     (sub1 value))
           (interval (add1 value)
                     (interval-end ival)))]
    [else
     (list ival)]))

(define (interval-subtract from ival)
  (cond
    [(not (intervals-overlap? from ival))
     (list from)]
    [(interval-contains? ival from)
     '()]
    [(interval-contains? from ival #:strict #t)
     (list (interval (interval-start from)
                     (sub1 (interval-start ival)))
           (interval (add1 (interval-end ival))
                     (interval-end from)))]
    [(<= (interval-start ival) (interval-end from) (interval-end ival))
     (list (interval (interval-start from)
                     (sub1 (interval-start ival))))]
    [(<= (interval-start ival) (interval-start from) (interval-end ival))
     (list (interval (add1 (interval-end ival))
                     (interval-end from)))]
    [else (error "Interval subtraction")]))

(define (cut-cover cover value)
  (append-map (curryr cut-interval value) cover))

(define (cover-subtract/interval cover ival)
  (append-map (curryr interval-subtract ival) cover))

(define (cover-complement super-cover cover)
  (foldl (λ (ival comp)
           (cover-subtract/interval comp ival))
         super-cover
         cover))

(define (make-cover unsorted-intervals)
  (let loop ([ivals (sort unsorted-intervals interval<?)])
    (cond
      [(empty? ivals) '()]
      [(empty? (cdr ivals)) ivals]
      [(intervals-overlap? (first ivals) (second ivals))
       (loop (merge/head ivals))]
      [else
       (cons (car ivals)
             (loop (cdr ivals)))])))

;; Parsing

(define (parse-point pt-str)
  (define coord-strings (regexp-match* #rx"-?[0-9]+" pt-str))
  (pt (string->number (first coord-strings))
      (string->number (second coord-strings))))

(define (parse-line line)
  (define point-strings (regexp-match* #rx"x=-?[0-9]+, y=-?[0-9]+" line))
  (sensor (parse-point (first point-strings))
          (parse-point (second point-strings))))

(define (parse-file filename)
  (map parse-line (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(define lower-bound (make-parameter -inf.0))
(define upper-bound (make-parameter +inf.0))

(define (distance l r)
  (define delta (pt- l r))
  (+ (abs (pt-x delta))
     (abs (pt-y delta))))

(define (distance-to-sensor the-sensor point)
  (distance (sensor-loc the-sensor) point))

(define (distance-to-closest-beacon the-sensor)
  (distance-to-sensor the-sensor (sensor-closest-beacon the-sensor)))

(define (distance-to-y the-sensor the-y)
  (abs (- (sensor-y the-sensor) the-y)))

(define (covered-locations/at-y the-sensor the-y)
  (define beacon-dist (distance-to-closest-beacon the-sensor))
  (define y-dist (distance-to-y the-sensor the-y))
  (define spaces-toward-sensor (- beacon-dist y-dist))
  (if (<= 0 spaces-toward-sensor)
      (let ([low (max (lower-bound)
                   (- (sensor-x the-sensor)
                      spaces-toward-sensor))]
            [high (min (upper-bound)
                    (+ (sensor-x the-sensor)
                       spaces-toward-sensor))])
        (list (interval low high)))
      (list)))

(module+ test
  (define s1 (sensor (pt 0 0) (pt 5 0)))
  (check-equal? (covered-locations/at-y s1 6)
                (set))
  (check-equal? (covered-locations/at-y s1 -6)
                (set))
  (check-equal? (covered-locations/at-y s1 5)
                (set (pt 0 5)))
  (check-equal? (covered-locations/at-y s1 3)
                (set (pt -2 3) (pt -1 3) (pt 0 3) (pt 1 3) (pt 2 3))))

(define (number-of-covered-spaces cover)
  (apply + (map interval-length cover)))

(define (cover-at-y sensors y)
  (make-cover (append-map (curryr covered-locations/at-y y) sensors)))

(define (part-1 input y)
  (define cover (cover-at-y input y))
  (define beacons (map sensor-closest-beacon input))
  (define beacons-at-y (filter (λ (b) (= (pt-y b) y)) beacons))
  (define xs (map pt-x beacons-at-y))
  (number-of-covered-spaces (foldl (λ (x cover) (cut-cover cover x)) cover xs)))

(part-1 ex-input 10)
(part-1 my-input 2000000)

;; Part 2

(define (domain) (list (interval (lower-bound) (upper-bound))))

(define (free-spaces-at-y sensors y)
  (define cover (cover-at-y sensors y))
  (cover-complement (domain) cover))

(define (tuning-freq x y)
  (+ (* x 4000000)
     y))

(define (part-2 input)
  (define free-spaces
    (foldl (λ (y so-far)
             (define free-spaces (free-spaces-at-y input y))
             (if (empty? free-spaces)
                 so-far
                 (cons (cons y free-spaces)
                       so-far)))
           '()
           (inclusive-range (lower-bound)
                            (upper-bound))))
  (define y (caar free-spaces))
  (define x (interval-start (cadar free-spaces)))
  (displayln (list x y))
  (tuning-freq x y))

(parameterize ([lower-bound 0]
               [upper-bound 20])
  (part-2 ex-input))

(parameterize ([lower-bound 0]
               [upper-bound 4000000])
  (part-2 my-input))
