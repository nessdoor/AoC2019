#lang racket

(require csv-reading)
(require "run-intcode.rkt")

(define fbloop ; given a list of phase settings, builds the amplifier loop and produces the output signal
  (lambda (code p-settings)
    (define-values (controller-in common-out) (make-pipe)) ; create the communication channels used for piping data around
    (define-values (common-in controller-out) (make-pipe))
    (define initializer ; initialize the amplifiers with their phase settings, constructing the loop via a queue of continuations
      (lambda (settings)
        (if (null? settings)
          '()
          (cons (call/cc
                  (lambda (init-cont)
                    (displayln (car settings) controller-out)
                    (run-intcode (vector-copy code) common-in common-out init-cont))) ; each continuation is an initialized amplifier
                (initializer (cdr settings))))))

    (define runner ; pass data around the amplifiers by administering inputs, executing their continuations and updating the execution queue
      (lambda (queue input)
        (if (continuation? (car queue)) ; an exhausted amplifier leaves behind a symbol, instead of a continuation
          (begin
            (displayln input controller-out)
            (runner (append (cdr queue) (list (call/cc (car queue)))) (string->number (read-line controller-in))))
          input))) ; as soon as an exhausted amplifier is dequeued, we know the rest of the loop has also been exhausted

    (runner (initializer p-settings) 0)))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0))])
  (displayln
    (let ([acs (list->vector (map string->number ((make-csv-reader input))))])
      (foldl (lambda (settings highest)
               (parameterize ([current-output-port (open-output-nowhere)]) ; silence the prompt
                 (max highest (fbloop acs settings))))
             0
             (permutations (range 5 10)))))
  (close-input-port input))
