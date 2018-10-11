; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; Modified version of r6rs/bytevectors.sld as a Chicken 4 module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (r6rs bytevectors)
;
; Bytevectors

(require-library srfi-4)
(require-library numbers)
(require-library utf8)

(module r6rs.bytevectors ()
  (import scheme)
  (import (only chicken include error void))
  (import numbers)
  (import utf8)
  (import foreign)

  (import
    (rename
      (only srfi-4
        make-u8vector
        u8vector?
        u8vector-length
        u8vector-ref
        u8vector-set!)
      ; do not rename make-u8vector
      (u8vector? bytevector?)
      (u8vector-length bytevector-length)
      (u8vector-ref bytevector-u8-ref)
      (u8vector-set! bytevector-u8-set!)))


  (export endianness native-endianness
          bytevector? make-bytevector bytevector-length
          bytevector-fill! r6rs:bytevector-copy! bytevector-copy
          bytevector-u8-ref bytevector-s8-ref
          bytevector-u8-set! bytevector-s8-set!
          bytevector-uint-ref bytevector-sint-ref
          bytevector-uint-set! bytevector-sint-set!
          bytevector-u16-ref bytevector-s16-ref
          bytevector-u16-set! bytevector-s16-set!
          bytevector-u16-native-ref bytevector-s16-native-ref
          bytevector-u16-native-set! bytevector-s16-native-set!
          bytevector-u32-ref bytevector-s32-ref
          bytevector-u32-set! bytevector-s32-set!
          bytevector-u32-native-ref bytevector-s32-native-ref
          bytevector-u32-native-set! bytevector-s32-native-set!
          bytevector-u64-ref bytevector-s64-ref
          bytevector-u64-set! bytevector-s64-set!
          bytevector-u64-native-ref bytevector-s64-native-ref
          bytevector-u64-native-set! bytevector-s64-native-set!
          bytevector=?
          bytevector-ieee-single-native-ref bytevector-ieee-single-ref
          bytevector-ieee-double-native-ref bytevector-ieee-double-ref
          bytevector-ieee-single-native-set! bytevector-ieee-single-set!
          bytevector-ieee-double-native-set! bytevector-ieee-double-set!
          r6rs:bytevector-copy! bytevector-copy
          bytevector->u8-list u8-list->bytevector
          bytevector->uint-list bytevector->sint-list
          uint-list->bytevector sint-list->bytevector

          utf8->string utf16->string utf32->string
          string->utf8 string->utf16 string->utf32)


  (define (bit-field n start end)
    (define (bitwise-not n) (- -1 n))
    (define (mask start end) (bitwise-not (arithmetic-shift -1 (- end start))))
    (bitwise-and (mask start end) (arithmetic-shift n (- start))))

  (define (octet n) (if (negative? n) (+ n 256) n))

  (define (make-bytevector length . maybe-fill)
    (if (null? maybe-fill)
      (make-u8vector length)
      (make-u8vector length (octet (car maybe-fill)))))
  
  (define (r6rs:error who what . irritants)
     (apply error what irritants))

  (include "r6rs/chicken-ieee.scm")
  (include "r6rs/r7rs-shim.scm")
  (include "r6rs/bytevectors-impl.scm")

)
