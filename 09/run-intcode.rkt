#lang racket

(provide run-intcode)

(define exit-cont 'nocont) ; if set, this continuation is called before any blocking procedure or after the Intcode program has exited
(define relative-base 0) ; initialize global relative base for relative mode
(define memory-vect 'noninit) ; delcare memory vector
(define growth-margin 10) ; how much additional space to allocate when growing memory vector

(define decoder ; return the d-th digit of n
  (lambda (n d)
    (if (= d 0)
        (remainder n 10)
        (decoder (quotient n 10) (- d 1)))))

(define memr ; memory read procedure
  (lambda (addr)
    (if (< addr (vector-length memory-vect))
      (vector-ref memory-vect addr)
      (begin
        (let ([oldmem memory-vect])
          (set! memory-vect (make-vector (+ addr growth-margin 1) 0))
          (vector-copy! memory-vect 0 oldmem))
        (vector-ref memory-vect addr)))))

(define memw ; memory write procedure
  (lambda (addr value)
    (memr addr) ; ugly growth trigger, but it was too easy
    (vector-set! memory-vect addr value)))

(define resolver ; decode the content of a parameter based on the m access mode
  (lambda (v i m)
    (case m
        ((0) (memr (memr i)))                      ; position
        ((1) (memr i))                             ; immediate
        ((2) (memr (+ relative-base (memr i))))))) ; relative

(define write-at ; write value at cell pointed by cell 'index'
  (lambda (index mode value)
    (if (zero? mode)
      (memw (memr index) value)                         ; position
      (memw (+ relative-base (memr index)) value))))    ; relative

(define alu ; execute an arithmetic op
  (lambda (v op ip m0 m1 m2)
    (write-at (+ 3 ip) m2
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
           (when (not (or (char-ready? input) (eq? 'nocont exit-cont))) ; if the reading procedure would block, try and call the exit-cont
             (set! exit-cont (call/cc (lambda (cc) (exit-cont cc)))))
           (write-at (+ 1 ip) m (string->number (read-line input))))
      ((4) (displayln (resolver v (+ 1 ip) m) output))
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
  (lambda (v op ip m0 m1 m2)
    (let ([p0 (resolver v (+ 1 ip) m0)]
          [p1 (resolver v (+ 2 ip) m1)])
      (write-at (+ 3 ip) m2
                (if (case op
                      ((7) (< p0 p1))
                      ((8) (= p0 p1)))
                  1
                  0)))
    (+ ip 4)))

(define rebase ; move the relative base
  (lambda (v ip m0)
    (set! relative-base (+ relative-base (resolver v (+ ip 1) m0)))
    (+ ip 2)))

(define run-intcode ; run an intcode program
  (lambda (v input-port output-port (cc 'nocont))
    (define ric-rec
      (lambda (ip)
        (let* ([instruction (memr ip)]
               [opcode (remainder instruction 100)]
               [mode0 (decoder instruction 2)]
               [mode1 (decoder instruction 3)]
               [mode2 (decoder instruction 4)])
          (unless (= 99 opcode) ; stop at opcode 99
            (ric-rec (case opcode
                            ((1 2) (alu v opcode ip mode0 mode1 mode2))             ; arithmetic ops branch
                            ((3 4) (io v opcode ip mode0 input-port output-port))   ; I/O branch
                            ((5 6) (branch v opcode ip mode0 mode1))                ; jumps branch
                            ((7 8) (compare v opcode ip mode0 mode1 mode2))         ; comparison branch
                            ((9) (rebase v ip mode0))                               ; adjust relative base
                            (else (error "Unexpected opcode: " opcode))))))))

    (set! memory-vect v)
    (set! exit-cont cc)
    (ric-rec 0)
    (when (not (eq? 'nocont exit-cont)) ; if exit-cont is set, call it as soon as the Intcode program stops
      (exit-cont 'nocont))))

