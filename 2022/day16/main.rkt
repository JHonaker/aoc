#lang racket

(require threading)

(struct valve (name flow tunnels) #:transparent)

;; Parsing

(define (parse-line line)
  (define name (~> (regexp-match #px"Valve [A-Z]{2}" line)
                   first
                   (substring 6)
                   string->symbol))
  (define flow-rate (~> (regexp-match #px"rate=\\d+" line)
                        first
                        (string-split "=")
                        second
                        string->number))
  (define tunnels (map string->symbol
                       (~> (~>> (first (regexp-match #px"valves? [A-Z, ]+" line))
                                (regexp-match #px"[A-Z, ]+"))
                           first
                           string-trim
                           (string-split ", "))))
  (valve name flow-rate tunnels))

(define (parse-file filename)
  (map parse-line (file->lines filename)))

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(define (mat-ref mat row col)
  (~> (vector-ref mat row)
      (vector-ref col)))

(define (mat-set! mat row col v)
  (~> (vector-ref mat row)
      (vector-set! col v)))

(define (make-mat dim v)
  (build-vector dim (λ _ (make-vector dim v))))

(define (labels->indices valves)
  (define (label->index label index the-valve)
    (define (subst v)
      (if (and (symbol? v)
               (symbol=? v label))
          index
          v))
    (valve (subst (valve-name the-valve))
           (valve-flow the-valve)
           (map subst (valve-tunnels the-valve))))
  (for/fold ([i 0]
             [valves valves]
             #:result valves)
            ([v (in-list valves)])
    (values (add1 i)
            (map (curry label->index (valve-name v) i) valves))))

(define (build-distance-matrix valves)
  (define N (length valves))
  (define dist-mat (make-mat N +inf.0))

  (for* ([v (in-list valves)]
         [edge (in-list (valve-tunnels v))])
    (mat-set! dist-mat (valve-name v) edge 1))

  (for ([i (in-range N)])
    (mat-set! dist-mat i i 0))

  (for* ([k (in-range N)]
         [i (in-range N)]
         [j (in-range N)])
    (define d-ij (mat-ref dist-mat i j))
    (define d-ijk (+ (mat-ref dist-mat i k)
                     (mat-ref dist-mat k j)))
    (when (> d-ij d-ijk)
      (mat-set! dist-mat i j d-ijk)))

  dist-mat)

(define (dfs-solver raw-input starting-time start-index)
  (define input (labels->indices raw-input))

  (define flow-hash
    (make-hash
     (map (λ (v) (cons (valve-name v)
                       (valve-flow v)))
          input)))
  (define dist-mat (build-distance-matrix input))

  (define (flow v) (hash-ref flow-hash v))
  (define (dist from to) (mat-ref dist-mat from to))

  (define nonzero-valves/list (filter
                               (compose not zero? flow)
                               (range (length input))))
  (define nonzero-valves/set (list->set nonzero-valves/list))

  ;; Uses indices not valve structs
  (define (dfs current-valve time-remaining open-valves)

    (if (or (<= time-remaining 0)
            (set=? open-valves nonzero-valves/set))
        0
        (let ([valid-moves (~>> nonzero-valves/list
                                (filter (compose not (curry set-member? open-valves)))
                                (filter (compose (curryr <= time-remaining)
                                                 (curry dist current-valve))))])
          (if (zero? (length valid-moves))
              0
              (apply max
               (map
                (λ (v)
                  (let ([time-remaining (- time-remaining
                                           (+ (dist current-valve v) 1))]
                        [open-valves (set-add open-valves v)])
                    (+ (* time-remaining (flow v))
                       (dfs v time-remaining open-valves))))
                valid-moves)))))


    )

  (dfs start-index starting-time (set start-index)))

(dfs-solver ex-input 30 0)
(dfs-solver my-input 30 47)

;; Part 2

(define (dfs-solver-2 raw-input starting-time start-index)
  (define input (labels->indices raw-input))

  (define flow-hash
    (make-hash
     (map (λ (v) (cons (valve-name v)
                       (valve-flow v)))
          input)))
  (define dist-mat (build-distance-matrix input))

  (define (flow v) (hash-ref flow-hash v))
  (define (dist from to) (mat-ref dist-mat from to))

  (define nonzero-valves/list (filter
                               (compose not zero? flow)
                               (range (length input))))
  (define nonzero-valves/set (list->set nonzero-valves/list))

  ;; Uses indices not valve structs
  (define (dfs left-valve left-time-remaining
               right-valve right-time-remaining
               total-time-remaining
               open-valves)

    (define (dfs-next left-next?)
      (define current-valve (if left-next? left-valve right-valve))
      (define valid-moves (~>> nonzero-valves/list
                              (filter (compose not (curry set-member? open-valves)))
                              (filter (compose (curryr <= total-time-remaining)
                                               (curry dist current-valve)))))
      (if (zero? (length valid-moves))
          0
          (apply max
           (map
            (λ (v)
              (let* ([cost-to-open (+ (dist current-valve v) 1)]
                     [time-remaining-after (- total-time-remaining
                                              cost-to-open)]
                    [open-valves (set-add open-valves v)])
                (+ (* time-remaining-after (flow v))
                   (if left-next?
                       (dfs v cost-to-open
                            right-valve right-time-remaining
                            total-time-remaining
                            open-valves)
                       (dfs left-valve left-time-remaining
                            v cost-to-open
                            total-time-remaining
                            open-valves)))))
            valid-moves))))

    (cond
      [(or (<= total-time-remaining 0)
           (set=? open-valves nonzero-valves/set))
       0]
      [(zero? left-time-remaining)
       (dfs-next #t)]
      [(zero? right-time-remaining)
       (dfs-next #f)]
      [else
       (let* ([min-time (min left-time-remaining
                             right-time-remaining)]
              [left-time (- left-time-remaining min-time)]
              [right-time (- right-time-remaining min-time)])
         (dfs left-valve left-time
              right-valve right-time
              (- total-time-remaining min-time)
              open-valves))]))

  (dfs start-index 0 start-index 0 starting-time (set start-index)))

(dfs-solver-2 ex-input 26 0)
(dfs-solver-2 my-input 26 47)
