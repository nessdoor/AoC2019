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
    (+ 4 ip)))

(define io ; execute an I/O instruction
  (lambda (v op ip m input output)
    (case op
      ((3) (display "> ")
           (let ([input (read input)])
             (if (exact-integer? input)
                 (vector-set! v (vector-ref v (+ 1 ip)) input)
                 (raise-user-error "Error: expected integer, given " input))))
      ((4) (newline)
           (write (resolver v (+ 1 ip) m) output))
      (else (error "Unexpected IO opcode: " op)))
    (+ 2 ip)))

(define branch ; execute a branch instruction
  (lambda (v op ip m0 m1)
    (let ([param (resolver v (+ 1 ip) m0)])
      (if (case op
            ((5) (not (zero? param)))
            ((6) (zero? param)))
          (resolver v (+ 2 ip) m1)
          (+ 3 ip)))))

(define compare ; execute a comparison instruction
  (lambda (v op ip m0 m1)
    (let ([p0 (resolver v (+ 1 ip) m0)]
          [p1 (resolver v (+ 2 ip) m1)])
      (vector-set! v
                   (vector-ref v (+ 3 ip))
                   (if (case op
                         ((7) (< p0 p1))
                         ((8) (= p0 p1)))
                       1
                       0)))
    (+ ip 4)))

(define run-intcode ; run an intcode program
  (lambda (v input-port output-port)
    (define ric-rec
      (lambda (ip)
        (let* ([instruction (vector-ref v ip)]
               [opcode (remainder instruction 100)]
               [mode0 (decoder instruction 2)]
               [mode1 (decoder instruction 3)]
               [mode2 (decoder instruction 4)])
          (unless (= 99 opcode) ; stop at opcode 99
            (ric-rec (case opcode
                            ((1 2) (alu v opcode ip mode0 mode1))                   ; arithmetic ops branch
                            ((3 4) (io v opcode ip mode0 input-port output-port))   ; I/O branch
                            ((5 6) (branch v opcode ip mode0 mode1))                ; jumps branch
                            ((7 8) (compare v opcode ip mode0 mode1))               ; comparison branch
                            (else (error "Unexpected opcode: " opcode))))))))

    (ric-rec 0)))

