;;;; Implementation of SRFI 4 u64vectors and s64vectors on top of vectors
;;; for Chicken 4, which does not support them even with the numbers egg

(define (make-bytevector size)
  (make-vector size 0))

;; Constructor

(define (make-u64vector size . maybe-value)
  (make+fill! 'u64 1 u64vector-length u64vector-set! size maybe-value))

(define (make-s64vector size . maybe-value)
  (make+fill! 's64 1 s64vector-length s64vector-set! size maybe-value))

;;; Variable-argument constructor

(define (u64vector . args)
  (->list u64vector-ref u64vector-length args))

(define (s64vector . args)
  (->list s64vector-ref s64vector-length args))

;; Predicate

(define (u64vector? obj)
  (and (multivector? obj) (eq? (tag obj) 'u64)))

(define (s64vector? obj)
  (and (multivector? obj) (eq? (tag obj) 's64)))

;; Length

(define (u64vector-length vec)
  (vector-length (bv vec)))

(define (s64vector-length vec)
  (vector-length (bv vec)))

;; Get element

(define (u64vector-ref vec index)
  (vector-ref (bv vec) index))

(define (s64vector-ref vec index)
  (vector-ref (bv vec) index))

;; Set element

(define (u64vector-set! vec index value)
  (vector-set! (bv vec) index value))

(define (s64vector-set! vec index value)
  (vector-set! (bv vec) index value))

;; List to vec

(define (u64vector->list vec)
  (->list u64vector-ref u64vector-length vec))

(define (s64vector->list vec)
  (->list s64vector-ref s64vector-length vec))

;; Vec to list

(define (list->u64vector list)
  (list-> make-u64vector u64vector-set! list))

(define (list->s64vector list)
  (list-> make-s64vector s64vector-set! list))

