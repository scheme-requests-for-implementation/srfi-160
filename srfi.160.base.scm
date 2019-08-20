(module (srfi 160 base) ()
  (import (scheme))
  (import (only (chicken base) include define-record-type case-lambda))
  (import (only (chicken module) export))
  (import (only (chicken bitwise) bitwise-and bitwise-ior
                           bitwise-not arithmetic-shift))

  ;; SRFI 4 versions of @vector->list don't accept start/end args
  (import (except (srfi 4)
    u8vector->list s8vector->list u16vector->list s16vector->list
    u32vector->list s32vector->list u64vector->list s64vector->list
    f32vector->list f64vector->list))

  (define (exact-integer? x) (and (exact? x) (integer? x)))
  (export
    make-u8vector make-s8vector make-u16vector make-s16vector
    make-u32vector make-s32vector make-u64vector make-s64vector
    make-f32vector make-f64vector make-c64vector make-c128vector )
  (export
    u8vector s8vector u16vector s16vector
    u32vector s32vector u64vector s64vector
    f32vector f64vector c64vector c128vector )
  (export
    u8vector? s8vector? u16vector? s16vector?
    u32vector? s32vector? u64vector? s64vector?
    f32vector? f64vector? c64vector? c128vector?) 
  (export
    u8vector-length s8vector-length u16vector-length s16vector-length
    u32vector-length s32vector-length u64vector-length s64vector-length
    f32vector-length f64vector-length c64vector-length c128vector-length)
  (export
    u8vector-ref s8vector-ref u16vector-ref s16vector-ref
    u32vector-ref s32vector-ref u64vector-ref s64vector-ref
    f32vector-ref f64vector-ref c64vector-ref c128vector-ref)
  (export
    u8vector-set! s8vector-set! u16vector-set! s16vector-set!
    u32vector-set! s32vector-set! u64vector-set! s64vector-set!
    f32vector-set! f64vector-set! c64vector-set! c128vector-set!)
  (export
    u8vector->list s8vector->list u16vector->list s16vector->list
    u32vector->list s32vector->list u64vector->list s64vector->list
    f32vector->list f64vector->list c64vector->list c128vector->list)
  (export
    list->u8vector list->s8vector list->u16vector list->s16vector
    list->u32vector list->s32vector list->u64vector list->s64vector
    list->f32vector list->f64vector list->c64vector list->c128vector)
  (export
    u8? s8? u16? s16? u32? s32? u64? s64? f32? f64? c64? c128?)

  (include "srfi/160/base/r7rec.scm")
  (include "srfi/160/base/complex.scm")
  (include "srfi/160/base/u8-vector2list.scm")
  (include "srfi/160/base/s8-vector2list.scm")
  (include "srfi/160/base/u16-vector2list.scm")
  (include "srfi/160/base/s16-vector2list.scm")
  (include "srfi/160/base/u32-vector2list.scm")
  (include "srfi/160/base/s32-vector2list.scm")
  (include "srfi/160/base/u64-vector2list.scm")
  (include "srfi/160/base/s64-vector2list.scm")
  (include "srfi/160/base/f32-vector2list.scm")
  (include "srfi/160/base/f64-vector2list.scm")
  (include "srfi/160/base/c64-vector2list.scm")
  (include "srfi/160/base/c128-vector2list.scm")
  (include "srfi/160/base/valid.scm")
)
