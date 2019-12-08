
#lang racket

(define climbing-digits ; check if digits are ever increasing
  (lambda (n d)
    (if (> d 0)
        (and (>= (remainder n 10) (remainder (quotient n 10) 10)) (climbing-digits (quotient n 10) (- d 1)))
        #t)))

(define isol-dig ; look for isolated digrams of equal digits
  (lambda (n d)
    (define is-rec ; use a recursive procedure to count occurences
      (lambda (n d last count)
        (if (< d 0)
            (= count 2) ; reached end with a count of 2: isolated digraph at the head
            (cond
              [(eqv? last (remainder n 10)) (is-rec (quotient n 10) (- d 1) last (+ 1 count))] ; increment count
              [(= count 2) #t] ; different character than the last one and (= count 2): found isolated digraph
              [else (is-rec (quotient n 10) (- d 1) (remainder n 10) 1)]))))

    (is-rec n d (void) 0)))

(let ([args (current-command-line-arguments)])
  (length (filter (lambda (n)
                    (and (climbing-digits n 5) (isol-dig n 5)))
                  (range
                    (string->number (vector-ref args 0))
                    (+ 1 (string->number (vector-ref args 1)))))))
