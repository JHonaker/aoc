#lang racket

;; Parsing

(define (parse-file filename)
  (define (parse-line line)
    (match (string-split line " ")
      [(list "noop")
       (list
        (list 'noop))]
      [(list "addx" val-str)
       (list
        (list 'noop)
        (list 'addx (string->number val-str)))]))
  (append-map parse-line (file->lines filename))
  )

(define ex-input (parse-file "ex-input"))
(define my-input (parse-file "my-input"))

;; Part 1

(struct cpu (clock reg-x) #:transparent)

;; Instead of waiting one cycle, just make addx expand to noop; addx where addx
;; takes one cycle

(define signal-log (list))
(define (clear-log!) (set! signal-log (list)))

(define (logging the-cpu times)
  (define log-time (member (cpu-clock the-cpu) times))
  (when log-time
    (define x (cpu-reg-x the-cpu))
    (printf "~a: x = ~a~%" (car log-time) x)
    (set! signal-log (cons (list (car log-time) x) signal-log)))
  the-cpu)

(define (advance-clock the-cpu)
  (cpu (add1 (cpu-clock the-cpu)) (cpu-reg-x the-cpu)))

(define init-cpu (cpu 0 1))

(define (instr-addx the-cpu value)
  (match-define (cpu clock x) the-cpu)
  (cpu clock (+ x value)))

(define (instr-noop the-cpu)
  (match-define (cpu clock x) the-cpu)
  (cpu clock x))

(define (interp-instr the-cpu instr watchers)
  (let ([the-cpu (logging (advance-clock the-cpu) watchers)])
    (define result
      (match instr
        [(list 'addx value) (instr-addx the-cpu value)]
        [(list 'noop) (instr-noop the-cpu)]))
    result))

(define (interp-program program [watchers (list)])
  (let loop ([the-cpu init-cpu]
             [program program])
    (if (empty? program)
        the-cpu
        (loop (interp-instr the-cpu (car program) watchers) (cdr program)))))

(define (signal-strength item)
  (* (first item)
     (second item)))

#; (interp-instr init-cpu (list 'addx 10) (list 0 1 2 3))
#; (interp-program '((noop)
                  (addx 3)
                  (addx -5)) (list 1 2 3))

(interp-program ex-input '(20 60 100 140 180 220))
(apply + (map signal-strength signal-log))
(clear-log!)
(interp-program my-input '(20 60 100 140 180 220))
(apply + (map signal-strength signal-log))

;; Part 2

(struct crt (pixels))
(struct device (the-cpu the-crt))

(define (visible? pix-x sprite-x)
  (<= -1 (- sprite-x pix-x) 1))

(define (draw-pixel/inner the-cpu the-crt)
  (match-define (cpu clock x) the-cpu)
  (define drawn-pixels (crt-pixels the-crt))
  (define pixel (if (visible? (- (modulo clock 40) 1) x)
                    'visible
                    'blank))
  ;(printf "C ~a, X=~a, V=~a~%" clock x pixel)
  (crt (cons pixel drawn-pixels)))

(define (draw-pixel the-device)
  (match-let ([(device the-cpu the-crt) the-device])
    (device the-cpu (draw-pixel/inner the-cpu the-crt))))

(define (advance-clock/device the-device)
  (match-let ([(device the-cpu the-crt) the-device])
    (device (advance-clock the-cpu) the-crt)))

(define (perform-instr the-device instr)
  (match-define (device the-cpu the-crt) the-device)
  (define result
    (match instr
        [(list 'addx value) (instr-addx the-cpu value)]
        [(list 'noop) (instr-noop the-cpu)]))
  (device result the-crt))

(define (display-device the-device)
  (define (vis->pixel vis)
    (match vis
      ['visible #\#]
      ['blank #\.]))
  (define (display-line pixels n)
    (map display (map vis->pixel (take pixels n)))
    (newline)
    (drop pixels n))
  (let loop ([pixels (reverse (crt-pixels (device-the-crt the-device)))])
    (unless (empty? pixels)
      (loop (display-line pixels 40)))))

(define (loop-device-once the-device instr)
  (let* ([the-device (advance-clock/device the-device)]
         [the-device (draw-pixel the-device)]
         [the-device (perform-instr the-device instr)])
    the-device))

(define (loop-device the-device program)
  (if (empty? program)
      the-device
      (loop-device (loop-device-once the-device (car program))
                   (cdr program))))

(define init-crt (crt '()))
(define init-device (device init-cpu init-crt))

(define ex-dev (loop-device init-device ex-input))
(display-device ex-dev)
(newline)
(define my-dev (loop-device init-device my-input))
(display-device my-dev)
