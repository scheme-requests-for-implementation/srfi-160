;;;; Implementation of SRFI 4-ish u1vectors

;; Utilities from SRFI 151

(define (bit-set? index n) 
  (not (zero? (bitwise-and (arithmetic-shift 1 index) n))))

(define (copy-bit index to bool)
  (if bool
      (bitwise-ior to (arithmetic-shift 1 index))
      (bitwise-and to (bitwise-not (arithmetic-shift 1 index)))))

(define (bytes len)
  (if (= (remainder len 8) 0)
    (quotient len 8)
    (+ (quotient len 8) 1)))

(define (byte i) (quotient i 8))
(define (bit i) (remainder i 8))

;;; Main constructor

(define (make-u1vector len . maybe-fill)
  (define vec (raw-make-u1vector len (make-u8vector (bytes len) 0)))
  (if (not (null? maybe-fill))
    (u1vector-simple-fill! vec (car maybe-fill)))
  vec)

;;; Variable-argument constructor

(define (u1vector . list)
  (list->u1vector list))

;; Predicate already defined

;; Length already defined

;; Get element

(define (bitify b) (if b 1 0))

(define (u1vector-ref vec i)
  (define bv (bv1 vec))
  (define offset (byte i))
  (define index (bit i))
  (define old (u8vector-ref bv offset))
  (bitify (bit-set? index old)))

;; Set element

(define (u1vector-set! vec i value)
  (define bv (bv1 vec))
  (define offset (byte i))
  (define index (bit i))
  (define old (u8vector-ref bv offset))
  (define newbit (if (zero? value) #f #t))
  (define new (copy-bit index old newbit))
  (u8vector-set! bv offset new))

;; List to vec

(define (list->u1vector list)
  (define len (length list))
  (define vec (make-u1vector len))
  (let loop ((i 0) (list list))
    (if (null? list)
      vec
      (begin
        (u1vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

;; Vec to list

(define (u1vector->list vec)
  (let loop ((i (- (u1vector-length vec) 1))
             (list '()))
    (if (< i 0)
      list
      (loop (- i 1) (cons (u1vector-ref vec i) list)))))

;; Simple fill (not exported)

(define (u1vector-simple-fill! vec value)
  (define len (u1vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (u1vector-set! vec i value)
        (loop (+ i 1))))))

