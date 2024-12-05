#lang racket

#|

First column: What opponent plays

A -> Rock
B -> Paper
C -> Scissors

Second column: What you should play

X -> Rock
Y -> Paper
Z -> Scissors

Scores based on shape you select:
- 1 for Rock
- 2 for Paper
- 3 for Scissors
Plus the score fo the outcome
- 0 if you lose
- 3 if you draw
- 6 if you win

|#

(define (losing-move opposing-move)
  (match opposing-move
    ['rock 'scissors]
    ['paper 'rock]
    ['scissors 'paper]))

(define (drawing-move opposing-move)
  opposing-move)

(define (winning-move opposing-move)
  (match opposing-move
    ['rock 'paper]
    ['paper 'scissors]
    ['scissors 'rock]))

(define (decode-opponent move)
  (match move
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]))

(define (decode-self move)
  (match move
    ["X" losing-move]
    ["Y" drawing-move]
    ["Z" winning-move]))

(define (decode-line move-string)
  (match (string-split move-string " ")
    [(list op-move my-move)
     (let* ([op-move (decode-opponent op-move)]
            [my-move-chooser (decode-self my-move)]
            [my-move (my-move-chooser op-move)])
       (list op-move my-move))]))

(define (decode-guide lines)
  (map decode-line lines))

(define (decode-file filename)
  (decode-guide (file->lines filename)))

(define test-input (decode-guide '("A Y" "B X" " C Z")))

(define my-input (decode-file "my-input"))

;;; Scoring

(define (opponent-move rps-round)
  (first rps-round))
(define (my-move rps-round)
  (second rps-round))

(define (move-selection-score-part rps-round)
  (match (my-move rps-round)
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (round-won-score-part rps-round)
  (match rps-round
    ['(rock paper) 6]
    ['(paper scissors) 6]
    ['(scissors rock) 6]
    [(list op me)
     (if (symbol=? op me)
         3
         0)]))

(define (score-round rps-round)
  (+ (move-selection-score-part rps-round)
     (round-won-score-part rps-round)))

(define (score-strategy rounds)
  (foldl + 0 (map score-round rounds)))

(score-strategy test-input)
(score-strategy my-input)
