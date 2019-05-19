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

(define (halfAdder a b)
  (let ((carry (and a b))
        (sum (xor a b)))
    (list carry sum)))

; test for halfAdder
(test-assert (equal? (list #f #f) (halfAdder #f #f)))
(test-assert (equal? (list #f #t) (halfAdder #f #t)))
(test-assert (equal? (list #f #t) (halfAdder #f #t)))
(test-assert (equal? (list #t #f) (halfAdder #t #t)))

(define-inline (carry-of adder)
  (list-ref adder 0))

(define-inline (sum-of adder)
  (list-ref adder 1))

(define (fullAdder a b c)
  (let ((halfAdder1 (halfAdder b c)))
    (let ((halfAdder2 (halfAdder (sum-of halfAdder1) a)))
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

(define (adder a b)
  (if (not (equal? (length a) (length b)))
    (display "List lengths not equal.\n")
    (letrec ((rec (lambda (x y c acc)
      (if (null? x)
        acc
        (if (null? c)
          (let ((halfResult (halfAdder (car x) (car y))))
            (rec (cdr x) (cdr y) (carry-of halfResult) (append acc (list (sum-of halfResult)))))
          (let ((fullResult (fullAdder (car x) (car y) c)))
            (rec (cdr x) (cdr y) (carry-of fullResult) (append acc (list (sum-of fullResult))))))))))
      (rec a b '() '()))))
