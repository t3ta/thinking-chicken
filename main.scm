(import test)

(define (xor a b)
  (or
    (and a (not b))
    (and (not a) b)))

; test for aor
(test #f (xor #t #t))
(test #t (xor #t #f))
(test #t (xor #f #t))
(test #f (xor #f #f))

(define (halfAdder a b c)
  (let ((carry (and a b))
        (sum (xor a b)))
    (list carry sum)))

; test for halfAdder
(test-assert (equal? (list #f #f) (halfAdder #f #f '())))
(test-assert (equal? (list #f #t) (halfAdder #f #t '())))
(test-assert (equal? (list #f #t) (halfAdder #f #t '())))
(test-assert (equal? (list #t #f) (halfAdder #t #t '())))

(define-inline (carry-of adder)
  (list-ref adder 0))

(define-inline (sum-of adder)
  (list-ref adder 1))

(define (fullAdder a b c)
  (let ((halfAdder1 (halfAdder b c '())))
    (let ((halfAdder2 (halfAdder (sum-of halfAdder1) a '())))
      (list (or (carry-of halfAdder1) (carry-of halfAdder2)) (sum-of halfAdder2)))))

; test for fullAdder
(test-assert (equal? (list #f #f) (fullAdder #f #f #f)))
(test-assert (equal? (list #f #t) (fullAdder #f #f #t)))
(test-assert (equal? (list #f #t) (fullAdder #f #t #f)))
(test-assert (equal? (list #t #f) (fullAdder #f #t #t)))
(test-assert (equal? (list #f #t) (fullAdder #t #f #f)))
(test-assert (equal? (list #t #f) (fullAdder #t #f #t)))
(test-assert (equal? (list #t #f) (fullAdder #t #t #f)))
(test-assert (equal? (list #t #t) (fullAdder #t #t #t)))

(define-inline (cdr-or-null lst)
  (if (null? lst) '() (cdr lst)))

(define-inline (car-or-false lst)
  (if (null? lst) #f (car lst)))

(define (adder a b)
  (letrec ((rec (lambda (x y c acc)
    (if (and (null? x) (null? y))
      acc
      (let ((result ((if (null? c) halfAdder fullAdder) (car-or-false x) (car-or-false y) c)))
          (rec (cdr-or-null x) (cdr-or-null y) (carry-of result) (append acc (list (sum-of result)))))))))
    (rec a b '() '())))

; test for adder
(test-assert (equal? (list #t #t #f) (adder (list #t #f #f) (list #f #t))))

(define (incrementer a)
  (adder a (list #t)))

; test for incrementer
(test-assert (equal? (list #f #t) (incrementer (list #t #f))))