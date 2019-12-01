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

(define rocket-equation
  (lambda (m)
    (- (floor (/ m 3)) 2)))

(define fuel-closure
  (lambda (f)
    (define fc-rec
      (lambda (p sum)
        (if (< p 0)
            sum
            (fc-rec (rocket-equation p) (+ p sum)))))

    (fc-rec f 0)))

(let ([pinput (open-input-file
                (vector-ref (current-command-line-arguments) 0)
                #:mode 'text)])
  (display (apply +
                  (map fuel-closure
                       (map
                         (lambda (s) (rocket-equation (string->number s)))
                         (input-to-list pinput)))))
  (newline)
  (close-input-port pinput))
