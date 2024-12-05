#lang racket

;; Parsing

(define (parse-file filename)
  (define (parse-line line)
    (filter-map string->number (string-split line "")))
  (let ([lines (file->lines filename)])
    (map parse-line lines)))

(define (m-transpose m)
  (apply map list m))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; All of our calculations will just deal with the "from the left" case
;; We can derive the other cases using row reversals and the transpose
;; If f operates on an entire row
;; Left - F(X)
;; Right - (reverse (F (reverse X)))
;; Top - (F(X'))'
;; Bot - (reverse (F((reverse X'))))'

(define (from-left f matrix . args)
  (for/list ([row (in-list matrix)])
    (apply f row args)))

(define (from-right f matrix . args)
  (for/list ([row (in-list matrix)])
    (reverse (apply f (reverse row) args))))

(define (from-top f matrix . args)
  (m-transpose
   (for/list ([row (in-list (m-transpose matrix))])
    (apply f row args))))

(define (from-bot f matrix . args)
  (m-transpose
   (for/list ([row (in-list (m-transpose matrix))])
     (reverse (apply f (reverse row) args)))))

;; Part 1

(define (visibility-from-left/row row)
  (for/fold ([tallest-so-far -1]
             [visibility '()]
             #:result (reverse visibility))
            ([height (in-list row)])
    (if (> height tallest-so-far)
        (values height (cons #t visibility))
        (values tallest-so-far (cons #f visibility)))))

(define (compute-visibility matrix)
  (map
   (λ (left right top bot)
     (map (lambda (l r t b) (or l r t b)) left right top bot))
   (from-left visibility-from-left/row matrix)
   (from-right visibility-from-left/row matrix)
   (from-top visibility-from-left/row matrix)
   (from-bot visibility-from-left/row matrix)))

(count identity (flatten (compute-visibility ex-input)))
(count identity (flatten (compute-visibility my-input)))

;; Part 2

(define (view-distance/row row)

  ;; The total distance is the sum of the distances to the most recent blocking
  ;; tree until you reach a blocker that is taller than the current tree
  ;;
  ;; blockers is a list of pairs of blocking trees seen so far
  ;; - Height of the blocking tree
  ;; - Distance to the closest previous blocking tree
  ;;
  ;; height is the height of the current tree
  ;;
  ;; rest-distance is the distance travelled since the view was blocked most
  ;; recently
  (define (compute-total-view-distance blockers height rest-distance)
    (let loop ([blockers blockers]
               [total-distance rest-distance])
      (if (empty? blockers)
        total-distance
        (match-let ([(cons closest-blocker-height
                           distance-to-next-blocker) (car blockers)])
          (if (> height closest-blocker-height)
              (loop (cdr blockers)
                    (+ distance-to-next-blocker
                       total-distance))
              total-distance)))))

  (for/fold ([blockers '()]
             [last-height -inf.0]
             [last-distance -1]
             [distances '()]
             #:result (reverse distances))
            ([height (in-list row)])
    (let ([blocked? (<= height last-height)])
      (if blocked?
          ;; If the view is blocked, then we need to remember the last tree as a
          ;; blocker
          (values (cons (cons last-height last-distance)
                        blockers)
                  height
                  1
                  (cons 1 distances))
          ;; If the view is not blocked, then our distance depends on the height
          ;; of the previous blocking trees
          (let ([view-distance (compute-total-view-distance
                                blockers
                                height
                                (+ last-distance 1))])
            (values blockers
                  height
                  (+ last-distance 1)
                  (cons view-distance distances)))))))

(define (scenic-score matrix)
  (map
   (λ (left right top bot)
     (map * left right top bot))
   (from-left view-distance/row matrix)
   (from-right view-distance/row matrix)
   (from-top view-distance/row matrix)
   (from-bot view-distance/row matrix)))

(apply max (flatten (scenic-score ex-input)))
(apply max (flatten (scenic-score my-input)))

