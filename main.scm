(import test)

(define-inline (carry-of adder)
  (list-ref adder 0))

(define-inline (sum-of adder)
  (list-ref adder 1))

(define-inline (cdr-or-null lst)
  (if (null? lst) '() (cdr lst)))

(define-inline (car-or-false lst)
  (if (null? lst) #f (car lst)))

(define (xor a b)
  (or
    (and a (not b))
    (and (not a) b)))

(display "- test for xor\n")
(test #f (xor #t #t))
(test #t (xor #t #f))
(test #t (xor #f #t))
(test #f (xor #f #f))
(display "\n")

(define (mux a b sel)
  (or (and a (not sel)) (and b sel)))

(define (not-n lst)
  (letrec ((rec (lambda (lst acc)
    (if (null? lst)
      acc
      (rec (cdr lst) (cons (not (car lst)) acc))))))
    (reverse (rec lst '()))))

(display "- test for not-n\n")
(test-assert (equal? (list #f #f #f #f #f #f #f #f) (not-n (list #t #t #t #t #t #t #t #t))))
(display "\n")

(define (and-n a b)
  (letrec ((rec (lambda (a b acc)
    (if (and (null? a) (null? b))
      acc
      (rec (cdr-or-null a) (cdr-or-null b) (cons (and (car-or-false a) (car-or-false b)) acc))))))
  (reverse (rec a b '()))))

(display "- test for and-n\n")
(test-assert (equal? (list #f #f #f #f #f #f #f #f) (and-n (list #t #t #t #t #t #t #t #t) (list #f #f #f #f #f #f #f #f))))
(display "\n")

(define (or-n a b)
  (letrec ((rec (lambda (a b acc)
    (if (and (null? a) (null? b))
      acc
      (rec (cdr-or-null a) (cdr-or-null b) (cons (or (car-or-false a) (car-or-false b)) acc))))))
  (reverse (rec a b '()))))

(display "- test for or-n\n")
(test-assert (equal? (list #t #t #t #t #t #t #t #t) (or-n (list #t #t #t #t #t #t #t #t) (list #f #f #f #f #f #f #f #f))))
(display "\n")

(display "- test for mux\n")
(test #f (mux #f #f #f))
(test #f (mux #f #t #f))
(test #t (mux #t #f #f))
(test #t (mux #t #t #f))
(test #f (mux #f #f #t))
(test #t (mux #f #t #t))
(test #f (mux #t #f #t))
(test #t (mux #t #t #t))
(display "\n")

(define (dMux in sel)
  (list (and in (not sel)) (and in sel)))

(display "- test for dMux\n")
(test-assert (equal? (list #t #f) (dMux #t #f)))
(test-assert (equal? (list #f #t) (dMux #t #t)))
(test-assert (equal? (list #f #f) (dMux #f #f)))
(test-assert (equal? (list #f #f) (dMux #f #f)))
(display "\n")

(define (halfAdder a b c)
  (let ((carry (and a b))
        (sum (xor a b)))
    (list carry sum)))

(display "- test for halfAdder\n")
(test-assert (equal? (list #f #f) (halfAdder #f #f '())))
(test-assert (equal? (list #f #t) (halfAdder #f #t '())))
(test-assert (equal? (list #f #t) (halfAdder #f #t '())))
(test-assert (equal? (list #t #f) (halfAdder #t #t '())))
(display "\n")

(define (fullAdder a b c)
  (let ((halfAdder1 (halfAdder b c '())))
    (let ((halfAdder2 (halfAdder (sum-of halfAdder1) a '())))
      (list (or (carry-of halfAdder1) (carry-of halfAdder2)) (sum-of halfAdder2)))))

(display "- test for fullAdder\n")
(test-assert (equal? (list #f #f) (fullAdder #f #f #f)))
(test-assert (equal? (list #f #t) (fullAdder #f #f #t)))
(test-assert (equal? (list #f #t) (fullAdder #f #t #f)))
(test-assert (equal? (list #t #f) (fullAdder #f #t #t)))
(test-assert (equal? (list #f #t) (fullAdder #t #f #f)))
(test-assert (equal? (list #t #f) (fullAdder #t #f #t)))
(test-assert (equal? (list #t #f) (fullAdder #t #t #f)))
(test-assert (equal? (list #t #t) (fullAdder #t #t #t)))
(display "\n")

(define (adder a b)
  (letrec ((rec (lambda (x y c acc)
    (if (and (null? x) (null? y))
      acc
      (let ((result ((if (null? c) halfAdder fullAdder) (car-or-false x) (car-or-false y) c)))
          (rec (cdr-or-null x) (cdr-or-null y) (carry-of result) (append acc (list (sum-of result)))))))))
    (rec a b '() '())))

(display "- test for adder\n")
(test-assert (equal? (list #t #f) (adder (list #f #f) (list #t #f))))
(test-assert (equal? (list #t #t #f) (adder (list #t #f #f) (list #f #t))))
(test-assert (equal? (list #t #t #f) (adder (list #f #t) (list #t #f #f))))
(test-assert (equal? (list #t #t #f #f #f #f #f #t ) (adder (list #f #t) (list #t #f #f #f #f #f #f #t))))
(display "\n")

(define (incrementer a)
  (adder a (list #t)))

(display "- test for incrementer\n")
(test-assert (equal? (list #f #t) (incrementer (list #t #f))))
(display "\n")