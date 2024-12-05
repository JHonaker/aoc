#lang racket

(define (parse-file filename)
  (map (compose string->symbol string)
       (string->list (car (file->lines filename)))))

(define (find-unique-run k xs)
  (let loop ([head (reverse (take xs k))]
             [tail (drop xs k)]
             [index 0])
    (define dup (check-duplicates head))
    (if dup
        (if (empty? tail)
            #f
            (loop (cons (car tail) (take head (sub1 k)))
                  (cdr tail)
                  (add1 index)))
        index)))

(define my-input (parse-file "my-input"))
(+ 4 (find-unique-run 4 my-input))
(+ 14 (find-unique-run 14 my-input))
