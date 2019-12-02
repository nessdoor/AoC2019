#lang racket

(require csv-reading)

(define slist-to-ivect
  (lambda (s)
    (list->vector (map string->number s))))

(define run-intcode
  (lambda (v)
    (define ric-rec
      (lambda (ip)
        (if (= 99 (vector-ref v ip))
          v
          (begin
            (vector-set! v
                         (vector-ref v (+ 3 ip))
                         ((case (vector-ref v ip)
                           ((1) +)
                           ((2) *)
                           (else 'undefined))
                          (vector-ref v (vector-ref v (+ 1 ip)))
                          (vector-ref v (vector-ref v (+ 2 ip)))))
            (ric-rec (+ 4 ip))))))

    (ric-rec 0)))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0)
               #:mode 'text)])
  (display (run-intcode (slist-to-ivect ((make-csv-reader input)))))
  (close-input-port input))
