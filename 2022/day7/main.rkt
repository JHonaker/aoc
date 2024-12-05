#lang racket

(struct symlink (name ref) #:transparent)
(struct file-data (name size) #:transparent)

(define (command? str)
  (char=? (string-ref str 0) #\$))

(define (parse-ls data location-stack)
  (if (empty? data)
      '()
      (match (string-split (car data) " ")
        [(list "dir" name)
         (let ([name (string->symbol name)])
           (cons (symlink name (cons name location-stack))
                 (parse-ls (cdr data) location-stack)))]
        [(list size name)
         (cons (file-data name (string->number size))
               (parse-ls (cdr data) location-stack))])))

(define (parse-log log-file
                   [location-stack '()]
                   [contents (make-immutable-hash)])
  (if (not (empty? log-file))
      (match (string-split (car log-file) " ")
        [(list "$" "cd" "..")
         (parse-log (cdr log-file)
                    (cdr location-stack)
                    contents)]
        [(list "$" "cd" dir-name)
         (parse-log (cdr log-file)
                    (cons (string->symbol dir-name)
                          location-stack)
                    contents)]
        [(list "$" "ls")
         (let*-values ([(ls-log rest-log) (splitf-at (cdr log-file)
                                                     (negate command?))]
                       [(folder-contents) (parse-ls ls-log location-stack)])
           (parse-log rest-log
                      location-stack
                      (hash-set contents location-stack folder-contents)))])
      contents))

(define ex-file-system (parse-log (file->lines "ex-input")))
(define my-file-system (parse-log (file->lines "my-input")))

(define (resolve-symlink file-system pointer)
  (define key (symlink-ref pointer))
  (hash-ref file-system key))

(define root-path '(/))
(define root-pointer (symlink '/ '(/)))

;; Part 1

(define (sum-file-system fs)
  (define cache (make-hash))
  (define (cached-value path)
    (hash-ref cache path (lambda _ #f)))
  (define (cache! path value)
    (hash-set! cache path value))
  (define (lookup-or-compute pointer)
    (match pointer
      [(symlink _ ref)
       (define value (cached-value ref))
       (unless value
         (let* ([sizes (map lookup-or-compute (resolve-symlink fs pointer))]
                [v (apply + sizes)])
           (cache! ref v)
           (set! value v)))
       value]
      [(file-data _ size) size]))
  (lookup-or-compute root-pointer)
  cache)

(define (part-1 fs)
  (define directory-sizes (hash->list (sum-file-system fs)))
  (define at-most-100000 (filter (lambda (x)
                                   (<= (cdr x) 100000))
                                 directory-sizes))
  (apply + (map cdr at-most-100000)))

(part-1 ex-file-system)
(part-1 my-file-system)

;; Part 2

(define ex-sizes (sum-file-system ex-file-system))
(define my-sizes (sum-file-system my-file-system))

(define total-disk-space 70000000)
(define required-disk-space 30000000)

(define (used-space fs)
  (hash-ref fs root-path))
(define (free-space fs)
  (- total-disk-space (used-space fs)))
(define (needed-space fs)
  (- required-disk-space (free-space fs)))

(define (part-2 fs)
  (define space-target (needed-space fs))
  (define candidates (filter (compose
                              (curry <= space-target)
                              cdr)
                             (hash->list fs)))
  (argmin cdr candidates))

(part-2 ex-sizes)
(part-2 my-sizes)
