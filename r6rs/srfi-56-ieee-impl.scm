;;; Copyright (c) 2004-2005 by Alex Shinn. Details at end of file.
;;; Heavily modified by John Cowan for SRFI 160

(define (bitwise-not a)
  (- -1 a))

;; from the SRFI-33 reference implementation
(define (extract-bit-field size position n)
  (bitwise-and (bitwise-not (arithmetic-shift -1 size))
               (arithmetic-shift n (- position))))

(define *default-float-endian* (native-endianness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax (with optional low-level macro definitions)

;; like let-optionals* but uses default if #f is specified
(define-syntax let-params*
  (syntax-rules ()
    ((_ ls () . body) (let () . body))
    ((_ ls ((var default) rest ...) . body)
     (let* ((tmp ls)
            (var (or (and (pair? tmp) (car tmp)) default)))
       (let-params* (if (pair? tmp) (cdr tmp) '()) (rest ...) . body)))
    ((_ ls ((var) rest ...) . body)
     (let-params* ls ((var #f) rest ...) . body))))

;; simplified version Oleg's good assert (no report:)
(define-syntax assert
  (syntax-rules ()
    ((_) #t)
    ((_ test rest ...)
     (let ((message (quote test)))
       (if (not test)
         ;;(error "" "test failed:" message) ; use this for bigloo
         (error "test failed:" message)
         (assert rest ...))))))

; (define-macro (assert . o) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library code.  Below here only optimization changes should be needed.

;; basic reading

(define (combine . bytes)
  (combine-ls bytes))

(define (combine-ls bytes)
  (let loop ((b bytes) (acc 0))
    (if (null? b) acc
        (loop (cdr b) (+ (arithmetic-shift acc 8) (car b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading floating point numbers

;; Inspired by Oleg's implementation from
;;   http://okmij.org/ftp/Scheme/reading-IEEE-floats.txt
;; but removes mutations and magic numbers and allows for manually
;; specifying the endianess.
;;
;; See also
;;   http://www.cs.auckland.ac.nz/~jham1/07.211/floats.html
;; and
;;   http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
;; as references to IEEE 754.

(define (read-ieee-float32 . opt)
  (let-params* opt ((port (current-input-port))
                    (endian *default-float-endian*))
    (define (mantissa expn b2 b3 b4)
      (case expn   ; recognize special literal exponents
        ((255)
         ;;(if (zero? (combine b2 b3 b4)) +/0. 0/0.) ; XXXX for SRFI-70
         #f)
        ((0)       ; denormalized
         (inexact (* (expt 2.0 (- 1 (+ 127 23))) (combine b2 b3 b4))))
        (else
         (inexact
          (* (expt 2.0 (- expn (+ 127 23)))
             (combine (+ b2 128) b3 b4)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4)
      (if (> b2 127)  ; 1st bit of b2 is low bit of expn
        (mantissa (+ (* 2 b1) 1) (- b2 128) b3 b4)
        (mantissa (* 2 b1) b2 b3 b4)))
    (define (sign b1 b2 b3 b4)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4) => -) (else #f))
        (exponent b1 b2 b3 b4)))
    (let* ((b1 (read-u8 port))  (b2 (read-u8 port))
           (b3 (read-u8 port))  (b4 (read-u8 port)))
      (if (eof-object? b4)
        b4
        (if (eq? endian 'big-endian)
          (sign b1 b2 b3 b4)
          (sign b4 b3 b2 b1))))))

(define (read-ieee-float64 . opt)
  (let-params* opt ((port (current-input-port))
                    (endian *default-float-endian*))
    (define (mantissa expn b2 b3 b4 b5 b6 b7 b8)
      (case expn   ; recognize special literal exponents
        ((255) #f) ; won't handle NaN and +/- Inf
        ((0)       ; denormalized
         (inexact (* (expt 2.0 (- 1 (+ 1023 52)))
                            (combine b2 b3 b4 b5 b6 b7 b8))))
        (else
         (inexact
          (* (expt 2.0 (- expn (+ 1023 52)))
             (combine (+ b2 16) b3 b4 b5 b6 b7 b8)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4 b5 b6 b7 b8)
      (mantissa (bitwise-ior (arithmetic-shift b1 4)          ; 7 bits
                             (extract-bit-field 4 4 b2))      ; + 4 bits
                (extract-bit-field 4 0 b2) b3 b4 b5 b6 b7 b8))
    (define (sign b1 b2 b3 b4 b5 b6 b7 b8)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4 b5 b6 b7 b8) => -)
              (else #f))
        (exponent b1 b2 b3 b4 b5 b6 b7 b8)))
    (let* ((b1 (read-u8 port))  (b2 (read-u8 port))
           (b3 (read-u8 port))  (b4 (read-u8 port))
           (b5 (read-u8 port))  (b6 (read-u8 port))
           (b7 (read-u8 port))  (b8 (read-u8 port)))
      (if (eof-object? b8)
        b8
        (if (eq? endian 'big-endian)
          (sign b1 b2 b3 b4 b5 b6 b7 b8)
          (sign b8 b7 b6 b5 b4 b3 b2 b1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing floating point numbers

;; Underflow rounds down to zero as in IEEE-754, and overflow gets
;; written as +/- Infinity.

;; Break a real number down to a normalized mantissa and exponent.
;; Default base=2, mant-size=23 (52), exp-size=8 (11) for IEEE singles
;; (doubles).
;;
;; Note: This should never be used in practice, since it can be
;; implemented much faster in C.  See decode-float in ChezScheme or
;; Gauche.

(define (call-with-mantissa&exponent num . opt)
  (define (last ls)
    (if (null? (cdr ls)) (car ls) (last (cdr ls))))
  (define (with-last&rest ls proc)
    (let lp ((ls ls) (res '()))
      (if (null? (cdr ls))
        (proc (car ls) (reverse res))
        (lp (cdr ls) (cons (car ls) res)))))
  (cond
    ((negative? num) (apply call-with-mantissa&exponent (- num) opt))
    ((zero? num) ((last opt) 0 0))
    (else
     (with-last&rest opt
       (lambda (proc params)
         (let-params* params ((base 2) (mant-size 23) (exp-size 8))
           (let* ((bot (expt base mant-size))
                  (top (* base bot)))
             (let loop ((n (inexact num)) (e 0))
               (cond
                 ((>= n top)
                  (loop (/ n base) (+ e 1)))
                 ((< n bot)
                  (loop (* n base) (- e 1)))
                 (else
                  (proc (exact (round n)) e)))))))))))

(define (write-ieee-float32 num . opt)
  (assert (real? num))
  (let-params* opt ((port (current-output-port))
                    (endian *default-float-endian*))
    (define (bytes)
      (call-with-mantissa&exponent num
        (lambda (f e)
          (let ((e0 (+ e 127 23)))
            (cond
              ((negative? e0)
               (let* ((f1 (exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (extract-bit-field 7 16 f1))        ; mant:16-23
                      (b3 (extract-bit-field 8 8 f1))         ; mant:8-15
                      (b4 (extract-bit-field 8 0 f1)))        ; mant:0-7
                 (list (if (negative? num) 128 0) b2 b3 b4)))
              ((> e0 255) ; XXXX here we just write infinity
               (list (if (negative? num) 255 127) 128 0 0))
              (else
               (let* ((b0 (arithmetic-shift e0 -1))
                      (b1 (if (negative? num) (+ b0 128) b0)) ; sign + exp:1-7
                      (b2 (bitwise-ior
                           (if (odd? e0) 128 0)               ; exp:0
                           (extract-bit-field 7 16 f)))       ;   + mant:16-23
                      (b3 (extract-bit-field 8 8 f))          ; mant:8-15
                      (b4 (extract-bit-field 8 0 f)))         ; mant:0-7
                 (list b1 b2 b3 b4))))))))
    (for-each
     (lambda (b) (write-u8 b port))
     (cond ((zero? num) '(0 0 0 0))
           ((eq? endian 'big-endian) (bytes))
           (else (reverse (bytes)))))))

(define (write-ieee-float64 num . opt)
  (assert (real? num))
  (let-params* opt ((port (current-output-port))
                    (endian *default-float-endian*))
    (define (bytes)
      (call-with-mantissa&exponent num 2 52 11
        (lambda (f e)
          (let ((e0 (+ e 1023 52)))
            (cond
              ((negative? e0)
               (let* ((f1 (exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (extract-bit-field 4 48 f1))
                      (b3 (extract-bit-field 8 40 f1))
                      (b4 (extract-bit-field 8 32 f1))
                      (b5 (extract-bit-field 8 24 f1))
                      (b6 (extract-bit-field 8 16 f1))
                      (b7 (extract-bit-field 8 8 f1))
                      (b8 (extract-bit-field 8 0 f1)))
                 (list (if (negative? num) 128 0) b2 b3 b4 b5 b6 b7 b8)))
              ((> e0 4095) ; infinity
               (list (if (negative? num) 255 127) 224 0 0 0 0 0 0))
              (else
               (let* ((b0 (extract-bit-field 7 4 e0))
                      (b1 (if (negative? num) (+ b0 128) b0))
                      (b2 (bitwise-ior (arithmetic-shift
                                        (extract-bit-field 4 0 e0)
                                        4)
                                       (extract-bit-field 4 48 f)))
                      (b3 (extract-bit-field 8 40 f))
                      (b4 (extract-bit-field 8 32 f))
                      (b5 (extract-bit-field 8 24 f))
                      (b6 (extract-bit-field 8 16 f))
                      (b7 (extract-bit-field 8 8 f))
                      (b8 (extract-bit-field 8 0 f)))
                 (list b1 b2 b3 b4 b5 b6 b7 b8))))))))
    (for-each
     (lambda (b) (write-u8 b port))
     (cond ((zero? num) '(0 0 0 0 0 0 0 0))
           ((eq? endian 'big-endian) (bytes))
           (else (reverse (bytes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Native IEEE (r6rs bytevector) procedures

(define *scratch* (make-bytevector 8))

(define (bytevector-ieee-single-native-ref bv k)
  (bytevector-copy! *scratch* 0 bv k 4)
  (let* ((port (open-input-bytevector *scratch*))
         (result (read-ieee-float32 port)))
    (close-input-port port)
    result))

(define (bytevector-ieee-double-native-ref bv k)
  (bytevector-copy! *scratch* 0 bv k 8)
  (let* ((port (open-input-bytevector *scratch*))
         (result (read-ieee-float64 port)))
    (close-input-port port)
    result))

(define (bytevector-ieee-single-native-set! bv k num)
  (let ((port (open-output-bytevector)))
    (write-ieee-float32 num port)
    (bytevector-copy! bv k (get-output-bytevector port))
    (close-output-port port)))

(define (bytevector-ieee-double-native-set! bv k num)
  (let ((port (open-output-bytevector)))
    (write-ieee-float64 num port)
    (bytevector-copy! bv k (get-output-bytevector port))
    (close-output-port port)))

;;; Copyright (c) 2004-2005 by Alex Shinn. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
