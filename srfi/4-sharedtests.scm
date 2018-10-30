;;;; Shared tests

;; Test for sameness

(define relerr (expt 2 -24))
(define (same? result expected)
  (cond
    ((and (number? result) (number? expected) (inexact? result) (inexact? expected))
     (let ((abserr (abs (* expected relerr))))
       (<= (- expected abserr) result (+ expected abserr))))
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
  (display label)
  (display " OK")
  (newline)
  value)

(define (test tag make-@vector @vector @vector? @vector-length
              @vector-ref @vector-set! @vector->list list->@vector low high)
  (display "STARTING ")
  (display tag)
  (display "vector TESTS:")
  (newline)
  (let* ((vec0 (create "make" (make-@vector 3)))
         (vec1 (create "make-fill" (make-@vector 3 3)))
         (vec2 (create "vec" (@vector 1 2 3)))
         (vec3 (create "from list" (list->@vector '(3 2 1))))
         (float (or (eq? tag 'f32) (eq? tag 'f64)))
         (zero (if float 0.0 0)))
    (is-same? (@vector? vec0) #t)
    (is-same? (@vector? vec1) #t)
    (is-same? (@vector? vec2) #t)
    (is-same? (@vector? vec3) #t)
    (is-same? (@vector-length vec0) 3)
    (is-same? (@vector-length vec1) 3)
    (is-same? (@vector-length vec2) 3)
    (is-same? (@vector-length vec3) 3)
    (@vector-set! vec0 0 low)
    (@vector-set! vec0 1 zero)
    (@vector-set! vec0 2 high)
    (is-same? (@vector-ref vec0 0) low)
    (is-same? (@vector-ref vec0 1) zero)
    (is-same? (@vector-ref vec0 2) high)
    (is-same? (@vector-ref vec1 0) 3)
    (is-same? (@vector-ref vec1 1) 3)
    (is-same? (@vector-ref vec1 2) 3)
    (is-same? (@vector-ref vec2 0) 1)
    (is-same? (@vector-ref vec2 1) 2)
    (is-same? (@vector-ref vec2 2) 3)
    (is-same? (@vector-ref vec3 0) 3)
    (is-same? (@vector-ref vec3 1) 2)
    (is-same? (@vector-ref vec3 2) 1)
    (is-same? (@vector->list vec0) (list low zero high))
    (is-same? (@vector->list vec1) '(3 3 3))
    (is-same? (@vector->list vec2) '(1 2 3))
    (is-same? (@vector->list vec3) '(3 2 1))))

(test 'u8 make-u8vector u8vector u8vector? u8vector-length
      u8vector-ref u8vector-set! u8vector->list list->u8vector
      0 255)

(test 's8 make-s8vector s8vector s8vector? s8vector-length
      s8vector-ref s8vector-set! s8vector->list list->s8vector
      -128 127)

(test 'u16 make-u16vector u16vector u16vector? u16vector-length
      u16vector-ref u16vector-set! u16vector->list list->u16vector
      0 65535)

(test 's16 make-s16vector s16vector s16vector? s16vector-length
      s16vector-ref s16vector-set! s16vector->list list->s16vector
      -32768 32767)

(test 'u32 make-u32vector u32vector u32vector? u32vector-length
      u32vector-ref u32vector-set! u32vector->list list->u32vector
      0 4294967295)

(test 's32 make-s32vector s32vector s32vector? s32vector-length
      s32vector-ref s32vector-set! s32vector->list list->s32vector
      -2147483648 2147483647)

(test 'u64 make-u64vector u64vector u64vector? u64vector-length
      u64vector-ref u64vector-set! u64vector->list list->u64vector
      0 18446744073709551615)

(test 's64 make-s64vector s64vector s64vector? s64vector-length
      s64vector-ref s64vector-set! s64vector->list list->s64vector
      -9223372036854775808 9223372036854775807)

(test 'f32 make-f32vector f32vector f32vector? f32vector-length
      f32vector-ref f32vector-set! f32vector->list list->f32vector
      -1e38 1e38)

(test 'f64 make-f64vector f64vector f64vector? f64vector-length
      f64vector-ref f64vector-set! f64vector->list list->f64vector
      -1e308 1e308)

