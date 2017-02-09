;; Ex 3.13
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; Ex 3.17

;; takes an X argument
(define count-pairs
  (let ((seen '()))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x seen) 0)
            (else (set! seen (cons x seen))
                  (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))

;; Ex 3.18 finding infinite lists

;; Checks if a list is infinite or not.
(define (check-infinity x)
  (define visited '())
  (define (is-infinite? x)
    (cond ((null? x) 'not-infinite)
          ((memq (car x) visited) 'infinite)
          (else (set! visited (cons (car x) visited))
                (is-infinite? (cdr x)))))
  (is-infinite? x))

;; Ex 3.19
;; Redo exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)
(define (has-cycle? xs)
  (define (seen-last-pair? x)
    (or (null? x) (null? (cdr x))))
  (define (chase turtle rabbit)
    (cond ((or (null? turtle) (null? rabbit)) #f)
          ((eq? (car turtle) (car rabbit)) #t)
          ((seen-last-pair? (cdr rabbit)) #f)
          (else (chase (cdr turtle) (cddr rabbit)))))
  (if (seen-last-pair? xs)
      #f
      (chase xs (cdr xs))))
