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

(define count-orbits ; recursively counts the orbits separating the key from COM
  (lambda (body)
    (define orb-rec
      (lambda (body acc)
        (if (equal? body COM)
          acc
          (orb-rec (hash-ref UOM body) (+ acc 1)))))

    (orb-rec body 0)))

(let ([input (open-input-file (vector-ref (current-command-line-arguments) 0))])
  (loader input)
  (display (foldl
             (lambda (body count) (+ (count-orbits body) count))
             0
             (hash-keys UOM)))
  (newline)
  (close-input-port input))
