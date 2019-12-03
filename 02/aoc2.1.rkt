#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(define slist-to-ivect
  (lambda (s)
    (list->vector (map string->number s))))

(let* ([args (current-command-line-arguments)]
       [input (open-input-file (vector-ref args 0))]
       [target (string->number (vector-ref args 1))])
  (let* ([code (slist-to-ivect ((make-csv-reader input)))]
         [temp (vector-copy code)])
    (let nloop ([noun 0])
      (let vloop ([verb 0])
        (vector-set! temp 1 noun)
        (vector-set! temp 2 verb)
        (run-intcode temp)
        (unless (or (= target (vector-ref temp 0)) (= 99 verb))
          (set! temp (vector-copy code))
          (vloop (+ 1 verb))))
      (unless (or (= target (vector-ref temp 0)) (= 99 noun))
        (set! temp (vector-copy code))
        (nloop (+ 1 noun))))
    (display (cons (vector-ref temp 1) (vector-ref temp 2)))
    (newline)
    (close-input-port input)))
