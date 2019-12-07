#lang racket

(require csv-reading)
(require "wire-analyzer.rkt")

(let* ([input (open-input-file (vector-ref (current-command-line-arguments) 0))]
       [reader (make-csv-reader input)])
  (let ([path1 (build-path (reader))]
        [path2 (build-path (reader))]
        [intersections (mutable-set)])
    (for-each (lambda (seg)
                (for-each (lambda (interset)
                            (set-union! intersections interset))
                          (map (lambda (collision)
                                 (list->set (find-intersections seg collision)))
                               (find-colliding-segments seg path2))))
              path1)
    (set-remove! intersections '(0 . 0))
    (display (mh-dist '(0 . 0) (nearest `(0 . 0) (set->list intersections)))))
  (close-input-port input))
