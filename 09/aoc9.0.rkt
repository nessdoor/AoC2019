#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0))])
  (run-intcode (list->vector (map string->number ((make-csv-reader input)))) (current-input-port) (current-output-port))
  (close-input-port input))
