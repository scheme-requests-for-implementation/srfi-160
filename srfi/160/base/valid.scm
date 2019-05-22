(define (u8? n) (and (exact-integer? n) (<= 0 n 255)))

(define (s8? n) (and (exact-integer? n) (<= -128 n 127)))

(define (u16? n) (and (exact-integer? n) (<= 0 n 65535)))

(define (s16? n) (and (exact-integer? n) (<= -32768 n 32767)))

(define (u32? n) (and (exact-integer? n) (<= 0 n 4294967295)))

(define (s32? n) (and (exact-integer? n) (<= -2147483648 n 2147483647)))

(define (u64? n) (and (exact-integer? n) (<= 0 n 18446744073709551615)))

(define (s64? n) (and (exact-integer? n) (<= -9223372036854775808 n 9223372036854775807)))

(define (f32? n) (and (inexact? n) (real? n)))

(define (f64? n) (f32? n))

(define (c64? n) (inexact? n))

(define (c128? n) (inexact? n))
