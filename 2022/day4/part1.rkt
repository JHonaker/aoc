#lang racket

(define (parse-assignment ass)
  (map string->number (string-split ass "-")))

(define (parse-line line)
  (map parse-assignment (string-split line ",")))

(define (parse-file filename)
  (map parse-line (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(define (range-contains? a b)
  (match-let ([(list low-a high-a) a]
              [(list low-b high-b) b])
    (and (<= low-a low-b)
         (<= high-b high-a))))

(define (either-contains? ass)
  (or (range-contains? (first ass) (second ass))
      (range-contains? (second ass) (first ass))))

(define (count-contains asses)
  (foldl (lambda (x acc) (+ acc (if x 1 0)))
         0
         (map either-contains? asses)))

(map either-contains? ex-input)

(count-contains ex-input)
(count-contains my-input)

;; Part 2

(define (range-disjoint? a b)
  (match-let ([(list low-a high-a) a]
              [(list low-b high-b) b])
    (or (< high-a low-b)
        (< high-b low-a))))

(define (range-overlaps? a b)
  (not (range-disjoint? a b)))

(define (range-overlaps?* ass)
  (range-overlaps? (first ass) (second ass)))

(define (count-overlapping asses)
  (foldl (lambda (x acc) (+ acc (if x 1 0)))
         0
         (map range-overlaps?* asses)))

(count-overlapping ex-input)
(count-overlapping my-input)
