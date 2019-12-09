#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0)
               #:mode 'text)])
  (run-intcode (list->vector
                 (map string->number
                      ((make-csv-reader input)))))
  (close-input-port input))
