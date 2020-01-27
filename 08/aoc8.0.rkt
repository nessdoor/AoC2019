#lang racket

(define slice-layer ; given an image size, slices the input into a list of layers
  (lambda (w h img)
    (define rec-slicer
      (lambda (r acc)
        (if (null? r)
          acc
          (rec-slicer (drop r (* w h)) (cons (take r (* w h)) acc)))))

    (rec-slicer img '())))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0))]
      [w (string->number (vector-ref (current-command-line-arguments) 1))]
      [h (string->number (vector-ref (current-command-line-arguments) 2))])
  (let ([res (foldl (lambda (lc m)              ; find the layer with the least amount of 0s
                      (if (< (car lc) (car m))
                        lc
                        m))
                    '(+inf.0 . (0 . 0))
                    (map (lambda (layer)
                          (foldl (lambda (pixel counter) ; for each layer, count the number of 0s, 1s and 2s it contains
                                  (case pixel
                                    ((#\0) (cons (add1 (car counter)) (cdr counter)))
                                    ((#\1) (cons (car counter) (cons (add1 (cadr counter)) (cddr counter))))
                                    ((#\2) (cons (car counter) (cons (cadr counter) (add1 (cddr counter)))))))
                                 '(0 . (0 . 0)) ; (0s . (1s . 2s))
                                 layer))
                         (slice-layer w h (string->list (read-line input)))))]) ; read the input string and slice it
    (displayln (* (cadr res) (cddr res)))) ; multiply the number of 1s and 2s and display them
  (close-input-port input))

