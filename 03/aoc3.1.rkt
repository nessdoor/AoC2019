#lang racket

(require csv-reading)
(require "wire-analyzer.rkt")

(define walker ; walks down a path given in string form, measuring distance from a set of points
  (lambda (path-string targets)
    (define rec-walk
      (lambda (dir steps pos left path dists)
        (when (and (set-member? targets pos) (not (hash-has-key? dists pos))) ; if the current point is an unprobed target,
            (hash-set! dists pos steps))                                      ; register its distance in the dictionary

        (when (and (= left 0) (not (null? path)))
              (begin                                 ; move onto the next segment
                (set! dir (string-ref (car path) 0))
                (set! left (string->number (substring (car path) 1)))
                (set! path (cdr path))))

        (if (= left 0)
          dists                                 ; our path must've been null in order to reach this point
          (rec-walk dir (+ steps 1) (case dir
                                      ((#\U) (cons (car pos) (+ 1 (cdr pos))))
                                      ((#\D) (cons (car pos) (- (cdr pos) 1)))
                                      ((#\L) (cons (- (car pos) 1) (cdr pos)))
                                      ((#\R) (cons (+ 1 (car pos)) (cdr pos))))
                    (- left 1) path dists))))

    (rec-walk #f 0 '(0 . 0) 0 path-string (make-hash))))

(define shortest-link ; take two dictionaries of intersections and distances and find the shortest link
  (lambda (d1 d2)
    (foldl (lambda (k minimum)
             (let ([dist (+ (dict-ref d1 k) (dict-ref d2 k))])
               (if (< dist minimum)
                   dist
                   minimum)))
           +inf.0 (dict-keys d1))))

(let* ([input (open-input-file (vector-ref (current-command-line-arguments) 0))]
       [reader (make-csv-reader input)])
  (let* ([pastring1 (reader)]
         [pastring2 (reader)]
         [path1 (build-path pastring1)]
         [path2 (build-path pastring2)]
         [intersections (mutable-set)])
    (for-each (lambda (seg)
                (for-each (lambda (interset)
                            (set-union! intersections interset))
                          (map (lambda (collision)
                                 (list->set (find-intersections seg collision)))
                               (find-colliding-segments seg path2))))
              path1)
    (set-remove! intersections '(0 . 0))
    (display (shortest-link (walker pastring1 intersections) (walker pastring2 intersections))))
  (close-input-port input))
