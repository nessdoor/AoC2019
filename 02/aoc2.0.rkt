#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(define slist-to-ivect
  (lambda (s)
    (list->vector (map string->number s))))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0)
               #:mode 'text)])
  (display (run-intcode (slist-to-ivect ((make-csv-reader input)))))
  (close-input-port input))
