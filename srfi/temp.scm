;;;; Implementation of SRFI 4 on top of (rnrs bytevectors)

;;; Main constructor

(define (make-u64vector size . maybe-value)
  (make+fill! 'u64 8 u64vector-length u64vector-set! size maybe-value))

(define (make-s64vector size . maybe-value)
  (make+fill! 's64 8 s64vector-length s64vector-set! size maybe-value))

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
  (/ (bytevector-length (bv vec)) 8))

(define (s64vector-length vec)
  (/ (bytevector-length (bv vec)) 8))

;; Get element

(define (u64vector-ref vec index)
  (bytevector-u64-native-ref (bv vec) (* index 8)))

(define (s64vector-ref vec index)
  (bytevector-s64-native-ref (bv vec) (* index 8)))

;; Set element

(define (u64vector-set! vec index value)
  (bytevector-u64-native-set! (bv vec) (* index 8) value))

(define (s64vector-set! vec index value)
  (bytevector-s64-native-set! (bv vec) (* index 8) value))

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

