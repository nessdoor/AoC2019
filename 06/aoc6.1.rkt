#lang racket

(define COM "COM")
(define UOM (make-hash)) ; it's a universal map, so it makes sense for it to be global

(define loader ; loads orbit relationships as "<value>)<key>" into the UOM
  (lambda (input)
    (let ([line (read-line input)])
      (unless (eof-object? line)
        (let ([orbit (string-split line ")")])
          (hash-set! UOM (cadr orbit) (car orbit))
          (loader input))))))

(define path-from-COM ; calculates the path from a celestial body to COM
  (lambda (body)
    (define finder-rec
      (lambda (body thread)
        (if (equal? body COM)
          (cons body thread)
          (finder-rec (hash-ref UOM body) (cons body thread)))))

    (finder-rec body '())))

(let ([input (open-input-file (vector-ref (current-command-line-arguments) 0))])
  (loader input)
  (close-input-port input)
  (let-values ([(y s) (drop-common-prefix                       ; calculate the paths to COM for
                        (path-from-COM (hash-ref UOM "YOU"))    ; both Santa and You. The length of
                        (path-from-COM (hash-ref UOM "SAN")))]) ; the paths not in common is the
    (display (+ (length y) (length s)))                         ; number of required transfers.
    (newline)))
