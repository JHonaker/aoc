#lang racket

(define (char->priority char)
  (define a-priority (char->integer #\a))
  (define A-priority (char->integer #\A))
  (define char-int (char->integer char))
  (if (<= a-priority char-int)
      (+ (- char-int a-priority)
         1)
      (+ (- char-int A-priority)
         27)))

(define (decode-line line)
  (let* ([lst (string->list line)])
    (let-values ([(one two) (split-at lst (/ (length lst) 2))])
      (list one two))))

(define (decode-file filename)
  (map decode-line (file->lines filename)))

(define (shared-item rucksack)
  (let ([one (first rucksack)]
        [two (second rucksack)])
    (first
     (set->list
      (set-intersect (list->set one)
                     (list->set two))))))

(apply + (map char->priority
              (map shared-item
                   (decode-file "ex-input"))))

(apply + (map char->priority
              (map shared-item
                   (decode-file "my-input"))))

;;; Part 2

(define (group-elves elves group-size)
  (let loop ([elves elves]
             [groups '()]
             [current-group '()]
             [current-size 0])
    (cond
      [(= group-size current-size)
          (loop elves
                (cons (reverse current-group) groups)
                '()
                0)]
      [(empty? elves) (reverse groups)]
      [else
          (loop (cdr elves)
                groups
                (cons (car elves) current-group)
                (add1 current-size))])))

(define (shared-item* . split-rucksacks)
  (define rucksacks (map (lambda (xs) (apply append xs)) split-rucksacks))
  (define sets (map list->set rucksacks))
  (first (set->list (foldl set-intersect (first sets) sets))))


(apply + (map char->priority
              (map (lambda (xs) (apply shared-item* xs))
                   (group-elves (decode-file "ex-input") 3))))
(apply + (map char->priority
              (map (lambda (xs) (apply shared-item* xs))
                   (group-elves (decode-file "my-input") 3))))
