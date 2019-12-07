#lang racket

(define climbing-digits ; check if digits are ever increasing
  (lambda (n d)
    (if (> d 0)
        (and (>= (remainder n 10) (remainder (quotient n 10) 10)) (climbing-digits (quotient n 10) (- d 1)))
        #t)))

(define dig ; look for digrams of equal digits
  (lambda (n d)
    (if (> d 0)
        (or (= (remainder n 10) (remainder (quotient n 10) 10)) (dig (quotient n 10) (- d 1)))
        #f)))

(let ([args (current-command-line-arguments)])
  (length (filter (lambda (n)
                    (and (climbing-digits n 5) (dig n 5)))
                  (range
                    (string->number (vector-ref args 0))
                    (+ 1 (string->number (vector-ref args 1)))))))
