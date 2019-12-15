#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(define pipeline ; given a list of phase settings, builds the amplifier loop and produces the output signal
  (lambda (code p-settings)
    (define-values (in-pipe out-pipe) (make-pipe)) ; amplifiers are piloted via an IO pipe
    (define rec-run ; this recursive executor commands a single amplifier instance at each recursion
      (lambda (ss in)
        (if (null? ss)
          in
          (begin
            (writeln (car ss) out-pipe)
            (writeln in out-pipe)
            (run-intcode (vector-copy code) in-pipe out-pipe)
            (rec-run (cdr ss) (read in-pipe))))))
    
    (rec-run p-settings 0)))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0))])
  (displayln
    (let ([acs (list->vector (map string->number ((make-csv-reader input))))])
      (foldl (lambda (settings highest)
               (parameterize ([current-output-port (open-output-nowhere)]) ; silence the prompt
                 (max highest (pipeline acs settings))))
             0
             (permutations (range 0 5)))))
  (close-input-port input))
