#lang racket

(provide run-intcode)

(define decoder ; return the d-th digit of n
  (lambda (n d)
    (if (= d 0)
        (remainder n 10)
        (decoder (quotient n 10) (- d 1)))))

(define resolver ; decode the content of a parameter based on the m access mode
  (lambda (v i m)
    (if (= m 1)
        (vector-ref v i)                   ; immediate
        (vector-ref v (vector-ref v i))))) ; position

(define alu ; execute an arithmetic op
  (lambda (v op ip m0 m1)
    (vector-set! v
                 (vector-ref v (+ 3 ip))
                 ((case op
                    ((1) +)
                    ((2) *)
                    (else 'undefined))
                  (resolver v (+ 1 ip) m0)
                  (resolver v (+ 2 ip) m1)))
    4))

(define io ; execute an I/O instruction
  (lambda (v op ip m)
    (case op
      ((3) (display "> ")
           (let ([input (read)])
             (if (exact-integer? input)
                 (vector-set! v (vector-ref v (+ 1 ip)) input)
                 (raise-user-error "Error: expected integer, given " input))))
      ((4) (write (resolver v (+ 1 ip) m))
           (newline))
      (else (error "Unexpected IO opcode: " op)))
    2))

(define run-intcode ; run an intcode program
  (lambda (v)
    (define ric-rec
      (lambda (ip)
        (let* ([instruction (vector-ref v ip)]
               [opcode (remainder instruction 100)]
               [mode0 (decoder instruction 2)]
               [mode1 (decoder instruction 3)]
               [mode2 (decoder instruction 4)])
          (unless (= 99 opcode) ; stop at opcode 99
            (ric-rec (+ ip (case opcode
                             ((1 2) (alu v opcode ip mode0 mode1)) ; arithmetic ops branch
                             ((3 4) (io v opcode ip mode0))        ; I/O branch
                             (else (error "Unexpected opcode: " opcode)))))))))

    (ric-rec 0)))

