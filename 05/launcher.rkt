#lang racket

(require csv-reading)

(let ([interpreter-path (vector-ref (current-command-line-arguments) 0)]
      [input (open-input-file
               (vector-ref (current-command-line-arguments) 1)
               #:mode 'text)])
  ((dynamic-require interpreter-path 'run-intcode) ; dynamically import the interpreter
   (list->vector (map string->number ((make-csv-reader input))))
   (current-input-port)
   (current-output-port))
  (close-input-port input))
