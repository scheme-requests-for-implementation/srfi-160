;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Bytevector operations that involve IEEE floating point.
; (from R6RS sample implementation)
;
; WARNING: The following code does not properly implement
; denormalized numbers.  A non-portable implementation that
; copies bytes directly between inexact number objects and
; bytevectors should be provided instead.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Replaced quotient and remainder'

; Magic numbers for IEEE-754 single and double precision:
;     the exponent bias (127 or 1023)
;     the integer value of the hidden bit (2^23 or 2^52)

(define bytevector:single-maxexponent 255)
(define bytevector:single-bias
  (quotient bytevector:single-maxexponent 2))
(define bytevector:single-hidden-bit 8388608)

(define bytevector:double-maxexponent 2047)
(define bytevector:double-bias
  (quotient bytevector:double-maxexponent 2))
(define bytevector:double-hidden-bit 4503599627370496)

; Given four exact integers, returns
;
;     (-1)^sign * (2^exponent) * p/q
;
; as an inexact real.

(define (bytevector:normalized sign exponent p q)
  (let* ((p/q (inexact (/ p q)))
         (x (* p/q (expt 2.0 exponent))))
    (cond ((= sign 0) x)
          ((= x 0.0) -0.0)
          (else (- x)))))

; Given exact positive integers p and q,
; returns three values:
; exact integers exponent, p2, and q2 such that
;     q2 <= p2 < q2+q2
;     p / q = (p2 * 2^exponent) / q2

(define (bytevector:normalized-ieee-parts p q)
  (cond ((< p q)
         (do ((p p (+ p p))
              (e 0 (- e 1)))
             ((>= p q)
              (values e p q))))
        ((<= (+ q q) p)
         (do ((q q (+ q q))
              (e 0 (+ e 1)))
             ((< p (+ q q))
              (values e p q))))
        (else
         (values 0 p q))))

; Given an inexact real x, an exponent bias, and an exact positive
; integer q that is a power of 2 representing the integer value of
; the hidden bit, returns three exact integers:
;
; sign
; biased-exponent
; p
;
; If x is normalized, then 0 < biased-exponent <= bias+bias,
; q <= p < 2*q, and
;
;     x = (-1)^sign * (2^(biased-exponent - bias)) * p/q
;
; If x is denormalized, then p < q and the equation holds.
; If x is zero, then biased-exponent and p are zero.
; If x is infinity, then biased-exponent = bias+bias+1 and p=0.
; If x is a NaN, then biased-exponent = bias+bias+1 and p>0.
;

(define (bytevector:ieee-parts x bias q)
  (cond ((nan? x)
         (values 0 (+ bias bias 1) (- q 1)))
        ((infinite? x)
         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
        ((zero? x)
         (values (if (eqv? x -0.0) 1 0) 0 0))
        (else
         (let* ((sign (if (negative? x) 1 0))
                (y (exact (abs x)))
                (num (numerator y))
                (den (denominator y)))
           (call-with-values
            (lambda () (bytevector:normalized-ieee-parts num den))
            (lambda (exponent num den)
              (let ((biased-exponent (+ exponent bias)))
                (cond ((< 0 biased-exponent (+ bias bias 1))
                       ; within the range of normalized numbers
                       (if (<= den q)
                           (let* ((factor (/ q den))
                                  (num*factor (* num factor)))
                             (if (integer? factor)
                                 (values sign biased-exponent num*factor)
                                 (r6rs:error 'bytevector:ieee-parts
                                        "this shouldn't happen: " x bias q)))
                           (let* ((factor (/ den q))
                                  (num*factor (/ num factor))
                                  (p (round num*factor)))
                             (if (< p (+ q q))
                                 (values sign
                                         biased-exponent
                                         p)
                                 (values sign
                                         (+ biased-exponent 1)
                                         (- p q))))))
                      ((>= biased-exponent (+ bias bias 1))
                       ; infinity
                       (values (if (positive? x) 0 1) (+ bias bias 1) 0))
                      (else
                       ; denormalized
                       ; FIXME: this has the double rounding bug
                       (do ((biased biased-exponent (+ biased 1))
                            (num (round (/ (* q num) den))
                                 (round (quotient num 2))))
                           ((and (< num q) (= biased 1))
                            (values sign biased num))))))))))))

; The exported procedures

; The definitions in this section should work in systems
; that use IEEE-754 double precision to represent inexact
; reals, and will probably work or come close to working
; with other floating point representations.  
; 
; None of the code in this section
; depends upon the bit-level representation of inexact
; reals.  (The code *does* depend upon the bit-level
; representation of IEEE-754 single and double precision,
; but that is an entirely different matter.)
;
; The representation-independent definitions in this section
; are far less efficient than a representation-dependent
; implementation would be.  For reasonable efficiency,
; the following procedures should be redefined:
;
; bytevector-ieee-single-native-ref
; bytevector-ieee-double-native-ref
; bytevector-ieee-single-native-set!
; bytevector-ieee-double-native-set!
;
; Since those four procedures are easier to implement in
; machine language, it did not seem worthwhile to try to
; optimize their semi-portable definitions here.
;

(define (bytevector-ieee-single-native-ref bytevector k)
  (let ((b0 (bytevector-u8-ref bytevector k))
        (b1 (bytevector-u8-ref bytevector (+ k 1)))
        (b2 (bytevector-u8-ref bytevector (+ k 2)))
        (b3 (bytevector-u8-ref bytevector (+ k 3))))
    (let ((sign (quotient b0 128))
          (exponent (+ (* 2 (remainder b0 128))
                       (quotient b1 128)))
          (fraction (+ (* 256 256 (remainder b1 128))
                       (* 256 b2)
                       b3)))
      (cond ((< 0 exponent bytevector:single-maxexponent)
             ; normalized (the usual case)
             (bytevector:normalized sign
                               (- exponent bytevector:single-bias)
                               (+ bytevector:single-hidden-bit fraction)
                               bytevector:single-hidden-bit))
            ((= 0 exponent)
             (cond ((> fraction 0)
                    ; denormalized
                    (bytevector:normalized sign
                                      (+ (- bytevector:single-bias) 1)
                                      fraction
                                      bytevector:single-hidden-bit))
                   ((= sign 0) 0.0)
                   (else -0.0)))
            ((= 0 fraction)
             (if (= sign 0) +inf.0 -inf.0))
            (else
             (if (= sign 0) +nan.0 -nan.0))))))

(define (bytevector-ieee-double-native-ref bytevector k)
  (let ((b0 (bytevector-u8-ref bytevector k))
        (b1 (bytevector-u8-ref bytevector (+ k 1)))
        (b2 (bytevector-u8-ref bytevector (+ k 2))))
    (let ((sign (quotient b0 128))
          (exponent (+ (* 16 (remainder b0 128))
                       (quotient b1 16)))
          (fraction (+ (* 281474976710656 (remainder b1 16))
                       (bytevector-uint-ref bytevector (+ k 2) 'big 6))))
      (cond ((< 0 exponent bytevector:double-maxexponent)
             ; normalized (the usual case)
             (bytevector:normalized sign
                               (- exponent bytevector:double-bias)
                               (+ bytevector:double-hidden-bit fraction)
                               bytevector:double-hidden-bit))
            ((= 0 exponent)
             (cond ((> fraction 0)
                    ; denormalized
                    (bytevector:normalized sign
                                      (+ (- bytevector:double-bias) 1)
                                      fraction
                                      bytevector:double-hidden-bit))
                   ((= sign 0) 0.0)
                   (else -0.0)))
            ((= 0 fraction)
             (if (= sign 0) +inf.0 -inf.0))
            (else
             (if (= sign 0) +nan.0 -nan.0))))))

(define (bytevector-ieee-single-native-set! bytevector k x)
  (call-with-values
   (lambda () (bytevector:ieee-parts x bytevector:single-bias bytevector:single-hidden-bit))
   (lambda (sign biased-exponent frac)
     (define (store! sign biased-exponent frac)
       (bytevector-u8-set! bytevector k
                            (+ (* 128 sign) (quotient biased-exponent 2)))
       (bytevector-u8-set! bytevector (+ k 1)
                            (+ (* 128 (remainder biased-exponent 2))
                               (quotient frac (* 256 256))))
       (bytevector-u8-set! bytevector (+ k 2)
                            (quotient (remainder frac (* 256 256)) 256))
       (bytevector-u8-set! bytevector (+ k 3)
                            (remainder frac 256))
       (unspecified))
     (cond ((= biased-exponent bytevector:single-maxexponent)
            (store! sign biased-exponent frac))
           ((< frac bytevector:single-hidden-bit)
            (store! sign 0 frac))
           (else
            (store! sign biased-exponent (- frac bytevector:single-hidden-bit)))))))

(define (bytevector-ieee-double-native-set! bytevector k x)
  (call-with-values
   (lambda ()
     (bytevector:ieee-parts x bytevector:double-bias
                            bytevector:double-hidden-bit))
   (lambda (sign biased-exponent frac)
     (define (store! sign biased-exponent frac)
       (bytevector-u8-set! bytevector k
                            (+ (* 128 sign)
                               (quotient biased-exponent 16)))
       (bytevector-u8-set! bytevector (+ k 1)
                            (+ (* 16 (remainder biased-exponent 16))
                               (quotient frac (* 65536 65536 65536))))
       (bytevector-u16-native-set! bytevector (+ k 2)
                              (quotient (remainder frac (* 65536 65536 65536))
                                         (* 65536 65536)))
       (bytevector-u32-native-set! bytevector (+ k 4)
                              (remainder frac (* 65536 65536)))
       (unspecified))
     (cond ((= biased-exponent bytevector:double-maxexponent)
            (store! sign biased-exponent frac))
           ((< frac bytevector:double-hidden-bit)
            (store! sign 0 frac))
           (else
            (store! sign biased-exponent (- frac bytevector:double-hidden-bit)))))))
