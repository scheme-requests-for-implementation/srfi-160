;;;; Implementation of SRFI 160 base c64vectors and c128vectors

;;; Main constructor

(define (make-c64vector len . maybe-fill)
  (define vec (raw-make-c64vector (make-f32vector (* len 2))))
  (if (not (null? maybe-fill))
    (c64vector-simple-fill! vec (car maybe-fill)))
  vec)

(define (make-c128vector len . maybe-fill)
  (define vec (raw-make-c128vector (make-f64vector (* len 2))))
  (if (not (null? maybe-fill))
    (c128vector-simple-fill! vec (car maybe-fill)))
  vec)

;;; Variable-argument constructor

(define (c64vector . list)
  (list->c64vector list))

(define (c128vector . list)
  (list->c128vector list))

;; Predicate already defined

;; Length

(define (c64vector-length vec)
  (/ (f32vector-length (bv64 vec)) 2))

(define (c128vector-length vec)
  (/ (f64vector-length (bv128 vec)) 2))

;; Get element

(define (c64vector-ref vec i)
  (let ((fvec (bv64 vec))
        (j (* i 2)))
    (make-rectangular
      (f32vector-ref fvec j)
      (f32vector-ref fvec (+ j 1)))))

(define (c128vector-ref vec i)
  (let ((fvec (bv128 vec))
        (j (* i 2)))
    (make-rectangular
      (f64vector-ref fvec j)
      (f64vector-ref fvec (+ j 1)))))

;; Set element

(define (c64vector-set! vec i value)
  (let ((fvec (bv64 vec))
        (j (* i 2)))
    (f32vector-set! fvec j (real-part value))
    (f32vector-set! fvec (+ j 1) (imag-part value))))

(define (c128vector-set! vec i value)
  (let ((fvec (bv128 vec))
        (j (* i 2)))
    (f64vector-set! fvec j (real-part value))
    (f64vector-set! fvec (+ j 1) (imag-part value))))

