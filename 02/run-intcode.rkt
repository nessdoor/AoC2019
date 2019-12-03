#lang racket

(provide run-intcode)

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

