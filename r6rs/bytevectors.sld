;; Note: this library is the same as (rnrs bytevectors (6))
;; except that it exports r6rs:bytevector-copy!
;; instead of bytevector-copy! (which has different argument order
;; in R7RS).

(define-library (r6rs bytevectors)
  (export
   endianness
   native-endianness

   bytevector? make-bytevector bytevector-length
   bytevector=?
   bytevector-fill! r6rs:bytevector-copy! bytevector-copy

   bytevector-u8-ref bytevector-s8-ref
   bytevector-u8-set! bytevector-s8-set!
   bytevector->u8-list u8-list->bytevector

   bytevector-uint-ref bytevector-sint-ref
   bytevector-uint-set! bytevector-sint-set!
   bytevector->uint-list bytevector->sint-list
   uint-list->bytevector sint-list->bytevector

   bytevector-u16-ref bytevector-s16-ref
   bytevector-u16-native-ref bytevector-s16-native-ref
   bytevector-u16-set! bytevector-s16-set!
   bytevector-u16-native-set! bytevector-s16-native-set!

   bytevector-u32-ref bytevector-s32-ref
   bytevector-u32-native-ref bytevector-s32-native-ref
   bytevector-u32-set! bytevector-s32-set!
   bytevector-u32-native-set! bytevector-s32-native-set!

   bytevector-u64-ref bytevector-s64-ref
   bytevector-u64-native-ref bytevector-s64-native-ref
   bytevector-u64-set! bytevector-s64-set!
   bytevector-u64-native-set! bytevector-s64-native-set!

   bytevector-ieee-single-native-ref
   bytevector-ieee-single-ref
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-set!
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-set!

   string->utf8 string->utf16 string->utf32
   utf8->string utf16->string utf32->string)

  (import
    (scheme base)
    (scheme inexact)
    (only (srfi 151) bitwise-and bitwise-ior arithmetic-shift bit-field))

  (begin
    (define (r6rs:error who what . irritants)
       (apply error what irritants))

    (define (infinite? x) (not (or (= x +inf.0) (= x -inf.0))))

    ;; define R6RS bytevector-copy! in terms of R7RS version
    (define (r6rs:bytevector-copy! source source-start target target-start count)
      (bytevector-copy! target target-start source source-start (+ source-start count)))
   
    ;; dummy native-ref procedures
;    (define (bytevector-ieee-single-native-ref bv k) 5.0)
;    (define (bytevector-ieee-double-native-ref bv k) 5.0)
;    (define (bytevector-ieee-single-native-set! bv k num) 5.0)
;    (define (bytevector-ieee-double-native-set! bv k num) 5.0)

  ) ; end begin

  (include "bytevectors-impl.scm")
  (include "srfi-56-ieee-impl.scm")

) ; r6rs bytevectors

