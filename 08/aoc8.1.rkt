#lang racket

(define slice-layer ; given an image size, slices the input into a list of layers
  (lambda (w h img)
    (define rec-slicer
      (lambda (r acc)
        (if (null? r)
          acc
          (rec-slicer (drop r (* w h)) (cons (take r (* w h)) acc)))))

    (rec-slicer img '())))

(define renderer ; given a list of layers, stacks them into an image
  (lambda (layers)
    (apply map ; apply map simultaneously on all the layers
           (lambda pixels
             (foldl ; stack pixels by folding through them
               (lambda (px visible) ; first layer is the bottom one
                 (if (eq? px #\2)
                   visible
                   px))
               'void
               pixels))
           layers)))

(let ([input (open-input-file
               (vector-ref (current-command-line-arguments) 0))]
      [w (string->number (vector-ref (current-command-line-arguments) 1))]
      [h (string->number (vector-ref (current-command-line-arguments) 2))])
  (display (renderer (slice-layer w h (string->list (read-line input)))))
  (close-input-port input))
