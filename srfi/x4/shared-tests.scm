;;;; Shared tests
;;; Hvector = homogeneous vector

;; Test for sameness

(define relerr (expt 2 -24))
(define (inexact-real? x) (and (number? x) (inexact? x) (real? x)))
(define (inexact-complex? x) (and (number? x) (inexact? x) (not (real? x))))
(define (realify z) (* (real-part z) (imag-part z)))

(define (same? result expected)
  (cond
    ((and (inexact-real? result) (inexact-real? expected))
     (let ((abserr (abs (* expected relerr))))
       (<= (- expected abserr) result (+ expected abserr))))
    ((and (inexact-complex? result) (inexact-complex? expected))
     (let ((abserr (abs (* (realify expected) relerr))))
       (<= (- (realify expected) abserr) (realify result) (+ (realify expected) abserr))))
    ((and (number? result) (number? expected))
     (= result expected))
    ((and (pair? result) (pair? expected))
     (list-same? result expected))
    (else
      (equal? result expected))))

 (define (list-same? result expected)
  (cond
    ((and (null? result) (null? expected))
     #t)
    ((and (pair? result) (pair? expected))
     (and (same? (car result) (car expected)) (list-same? (cdr result) (cdr expected))))
    (else
     #f)))

(define-syntax is-same?
  (syntax-rules ()
    ((what result expected)
     (begin
       (display "Try ")
       (display 'result)
       (display " is same as ")
       (display 'expected)
       (display "? ")
       (if (same? result expected)
         (display "OK")
         (begin
           (display result)
           (display " ")
           (display expected)
           (display " FAIL")))
       (newline)))))

(define (create label value)
  value)

(define (test tag make-Hvector Hvector Hvector? Hvector-length
              Hvector-ref Hvector-set! Hvector->list list->Hvector)
  (display "STARTING ")
  (display tag)
  (display "vector TESTS:")
  (newline)
  (let* ((bit (eq? tag 'u1))
         (first (if bit 1 32.0))
         (second (if bit 0 32.0+47.0i))
         (third (if bit 1 -47.0i))
         (vec0 (make-Hvector 3))
         (vec1 (make-Hvector 3 second))
         (vec2 (Hvector first second third))
         (vec3 (list->Hvector (list third second first))))
    (is-same? (Hvector? vec0) #t)
    (is-same? (Hvector? vec1) #t)
    (is-same? (Hvector? vec2) #t)
    (is-same? (Hvector? vec3) #t)
    (is-same? (Hvector-length vec0) 3)
    (is-same? (Hvector-length vec1) 3)
    (is-same? (Hvector-length vec2) 3)
    (is-same? (Hvector-length vec3) 3)
    (Hvector-set! vec0 0 second)
    (Hvector-set! vec0 1 third)
    (Hvector-set! vec0 2 first)
    (is-same? (Hvector-ref vec0 0) second)
    (is-same? (Hvector-ref vec0 1) third)
    (is-same? (Hvector-ref vec0 2) first)
    (is-same? (Hvector-ref vec1 0) second)
    (is-same? (Hvector-ref vec1 1) second)
    (is-same? (Hvector-ref vec1 2) second)
    (is-same? (Hvector-ref vec2 0) first)
    (is-same? (Hvector-ref vec2 1) second)
    (is-same? (Hvector-ref vec2 2) third)
    (is-same? (Hvector-ref vec3 0) third)
    (is-same? (Hvector-ref vec3 1) second)
    (is-same? (Hvector-ref vec3 2) first)
    (is-same? (Hvector->list vec0) (list second third first))
    (is-same? (Hvector->list vec1) (list second second second))
    (is-same? (Hvector->list vec2) (list first second third))
    (is-same? (Hvector->list vec3) (list third second first))))

(test 'u1 make-u1vector u1vector u1vector? u1vector-length
      u1vector-ref u1vector-set! u1vector->list list->u1vector)

(test 'c64 make-c64vector c64vector c64vector? c64vector-length
      c64vector-ref c64vector-set! c64vector->list list->c64vector)

(test 'c128 make-c128vector c128vector c128vector? c128vector-length
      c128vector-ref c128vector-set! c128vector->list list->c128vector)

