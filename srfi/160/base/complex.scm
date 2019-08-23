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

;; Simple fill! (not exported)

(define (c64vector-simple-fill! vec value)
  (define len (c64vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (c64vector-set! vec i value)
        (loop (+ i 1))))))

(define (c128vector-simple-fill! vec value)
  (define len (c128vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (c128vector-set! vec i value)
        (loop (+ i 1))))))

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

;; List to vec

(define (list->c64vector list)
  (define len (length list))
  (define vec (make-c64vector len))
  (let loop ((i 0) (list list))
    (if (= i len)
      vec
      (begin
        (c64vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

(define (list->c128vector list)
  (define len (length list))
  (define vec (make-c128vector len))
  (let loop ((i 0) (list list))
    (if (= i len)
      vec
      (begin
        (c128vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

;; Vec to list defined in at-vector2list

