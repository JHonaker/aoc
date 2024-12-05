#lang racket

(define test-input
  (list 1000
        2000
        3000
        #f
        4000
        #f
        5000
        6000
        #f
        7000
        8000
        9000
        #f
        10000))

(define (process-input filename)
  (let ([lines (file->lines filename)])
    (append
     (map (lambda (x)
            (if (zero? (string-length x))
                #f
                (string->number x)))
          lines)
     (list #f))))

(define (count-calories items)
  (let loop ([items items]
             [current-total 0]
             [elf-totals '()])
    (match items
      ['()
       (reverse elf-totals)]
      [(cons #f rest-items)
       (loop rest-items
             0
             (cons current-total elf-totals))]
      [(cons current-item rest-items)
       (loop rest-items
             (+ current-item current-total)
             elf-totals)])))

(define my-input (process-input "my-input"))
(define elves (count-calories my-input))

(argmax cdr (map (λ (i x) (cons i x))
                 (range (length elves))
                 elves))

(define (shift-vector-right! vec i value)
  (let loop ([j (sub1 (vector-length vec))])
    (vector-set! vec j (if (= j i)
                           value
                           (vector-ref vec (sub1 j))))
    (if (> j i)
        (loop (sub1 j))
        vec)))

(define (top-k k xs)
  (define ks (make-vector k 0))
  (let loop ([xs xs])
    (cond
      [(empty? xs) ks]
      [else
       (let ([x (car xs)]
             [lowest-k (vector-ref ks (sub1 k))]
             )
         (cond
           [(<= x lowest-k)
              (loop (cdr xs))]
           [else
            (let ([n-lower (apply + (map (λ (k) (if (< k x) 1 0)) (vector->list ks)))])
              (shift-vector-right! ks (- k n-lower) x)
              (loop (cdr xs)))]))])))
