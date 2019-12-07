#lang racket

(provide build-path find-intersections find-colliding-segments nearest mh-dist)

(require racket/bool)

(define vertical?
  (lambda (segment)
    (or (eq? (car segment) 'down) (eq? (car segment) 'up))))

(define horizontal?
  (lambda (segment)
    (not (vertical? segment))))

(define perpendicular?
  (lambda (s1 s2)
    (xor (vertical? s1) (vertical? s2))))

(define mh-dist
  (lambda (p1 p2)
    (+ (abs (- (car p1) (car p2))) (abs (- (cdr p1) (cdr p2))))))

(define signeq? ; returns true when all the supplied numbers are of the same sign (or zero)
  (lambda (n . rest)
    (if (null? rest)
        #t
        (and
          (or
            (and (positive? n) (positive? (car rest)))
            (and (negative? n) (negative? (car rest)))
            (and (zero? n) (zero? (car rest))))
          (apply signeq? rest)))))

(define disjunct? ; returns true when two intervals are disjunct
  (lambda (i1 i2)
    (let ([ac (- (car i2) (car i1))]
          [ad (- (cdr i2) (car i1))]
          [bc (- (car i2) (cdr i1))]
          [bd (- (cdr i2) (cdr i1))])
      (and
        (not (or (zero? ac) (zero? ad) (zero? bc) (zero? bd)))
        (signeq? ac ad bc bd)))))

(define interval-intersect ; returns the intersection of two intervals
  (lambda (i1 i2)
    (let ([start (if (> (car i1) (car i2))
                     (car i1)
                     (car i2))]
          [end (if (> (cdr i1) (cdr i2))
                       (cdr i2)
                       (cdr i1))])
      (cons start end))))

(define find-colliding-segments ; finds all the segments colliding with the given one
  (lambda (seg candidates)
    (filter
      (lambda (c)
        (not (or (disjunct? (cadr seg) (cadr c)) (disjunct? (cddr seg) (cddr c)))))
      candidates)))

(define find-intersections ; given two colliding segments, finds their intersection points
  (lambda (s1 s2)
    (if (perpendicular? s1 s2)   ; if the two colliding segments are perpendicular, then
        (cons (if (vertical? s1) ; the intersection is one and only one
                  (cons (caadr s1) (caddr s2))
                  (cons (caadr s2) (caddr s1)))
              '())
        (let ([intersect #f])
          (if (vertical? s1) ; since in this case segments overlap, we have a host of intersection points,
              (begin         ; all sharing one of the two coordinates
                (set! intersect (interval-intersect (cddr s1) (cddr s2)))
                (foldl
                  (lambda (y acc) (cons (cons (caadr s1) y) acc))
                  '()
                  (range (car intersect) (+ 1 (cdr intersect)))))
              (begin
                (set! intersect (interval-intersect (cadr s1) (cadr s2)))
                (foldl
                  (lambda (x acc) (cons (cons x (caddr s1)) acc))
                  '()
                  (range (car intersect) (+ 1 (cdr intersect))))))))))

(define nearest ; finds the point nearest to its first argument
  (lambda (o pl)
    (define near-rec
      (lambda (np mind rem)
        (if (null? rem)
            np
            (let ([distance (mh-dist o (car rem))])
              (if (< distance mind)
                 (near-rec (car rem) distance (cdr rem))
                 (near-rec np mind (cdr rem)))))))

    (near-rec '() +inf.0 pl)))

(define string->segment ; converts a "specification" string into a segment
  (lambda (start spec)
    (let ([direction (case (string-ref spec 0)
                       ((#\U) 'up)
                       ((#\D) 'down)
                       ((#\L) 'left)
                       ((#\R) 'right))]
          [distance (string->number (substring spec 1))])
      (cons direction       ; segments are represented as a triplet of direction and two intervals representing
            (case direction ; the horizontal and vertical space occupied: (<direction> (<ax,bx>) . <cy,dy>)
              ((up) (cons (cons (car start) (car start)) (cons (cdr start) (+ distance (cdr start)))))
              ((down) (cons (cons (car start) (car start)) (cons (- (cdr start) distance) (cdr start))))
              ((left) (cons (cons (- (car start) distance) (car start)) (cons (cdr start) (cdr start))))
              ((right) (cons (cons (car start) (+ distance (car start))) (cons (cdr start) (cdr start)))))))))

(define build-path ; builds a path as a list of segments
  (lambda (sl)
    (define bp-rec
      (lambda (sl start)
        (if (null? sl)
            '()
            (let ([new_segment (string->segment start (car sl))])
              (cons new_segment (bp-rec (cdr sl) (case (car new_segment)
                                                   ((up) (cons (caadr new_segment) (cdddr new_segment)))
                                                   ((down) (cons (caadr new_segment) (caddr new_segment)))
                                                   ((left) (cons (caadr new_segment) (caddr new_segment)))
                                                   ((right) (cons (cdadr new_segment) (caddr new_segment))))))))))

  (bp-rec sl '(0 . 0))))

