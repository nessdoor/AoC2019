#lang racket

(define input-to-list
  (lambda (iport)
    (define itl-rec
      (lambda (iport acc)
        (let ([line (read-line iport)])
          (if (eof-object? line)
              acc
              (itl-rec iport
                       (cons
                         line
                         acc))))))

    (itl-rec iport '())))

(let ([pinput (open-input-file
                (vector-ref (current-command-line-arguments) 0)
                #:mode 'text)])
  (display
    (apply +
           (map (lambda (s)
                  (- (floor (/ (string->number s) 3)) 2))
                (input-to-list pinput))))
  (newline)
  (close-input-port pinput))
