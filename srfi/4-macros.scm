;;;; Macros for SRFI-4 and extended SRFI-4 implementation

;; The guts of a make-@vector without a fill.
;; Requires an @vector type tag and a multiplier for the size
;; that says how many bytes are needed to represent an element.

(define-syntax make
  (syntax-rules ()
    ((make tag mult size)
     (make-multivector tag (make-bytevector (* mult size))))))

;; The guts of a routine to fill a vector with a single value.
;; Requires @vector-set! and @vector-length

(define-syntax fill!
  (syntax-rules ()
    ((fill! vlen vset! vec value)
     (let loop ((i 0))
       (unless (= i (vlen vec))
         (vset! vec i value)
         (loop (+ i 1)))))))

;; Combined make and fill! macro.
;; Requires everything that each submacro requires

(define-syntax make+fill!
  (syntax-rules ()
    ((make+fill! tag mult vlen vset size maybe-value)
     (let ((vec (make tag mult size)))
       (if (not (null? maybe-value))
         (fill! vlen vset vec (car maybe-value)))
       vec))))

;; The guts of an @vector->list converter.
;; Requires @vector-length and @vector-ref

(define-syntax ->list
  (syntax-rules ()
    ((->list vref vlength vec)
     (let loop ((i (- (vlength vec) 1)) (r '()))
       (if (< i 0)
         r
         (loop (- i 1) (cons (vref vec i) r)))))))

;; The guts of a list->@vector converter.
;; Requires make-@vector and @vector-set!

(define-syntax list->
  (syntax-rules ()
    ((list-> vmake vset! list)
     (let* ((len (length list))
            (vec (vmake len)))
       (let loop ((i 0) (list list))
         (unless (= i len)
           (vset! vec i (car list))
           (loop (+ i 1) (cdr list)))
         vec)))))

