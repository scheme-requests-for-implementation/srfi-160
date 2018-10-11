; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library --  bytevectors.
;
; Changes from Larceny 1.3:
; Removed native IEEE functions (see ieee-impl.scm)
; Removed $trace call
; Removed traditional Larceny procedures
; Removed local bitwise-* definitions and altered call sites
; Removed R7RS-specific procedures (see r7rs-shim.scm)
; Changed references to bytevector-ref and bytevector-set!
; Replaced endianness macro, changed native-endianness procedure
; Added definition of assertion-violation
; Replaced quotient and remainder with quotient and remainder
; Defined "unspecified"
; Fixed typo in s8-list->bytevector

(define (unspecified) (if #f #f))


(define-syntax endianness
  (syntax-rules (big little)
    ((endianness big) 'big)
    ((endianness little) 'little)))

(define (assertion-violation who what . irritants)
  (apply r6rs:error who what irritants))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Bytevector procedures provided by R6RS.
; FIXME:  These should be bummed for performance.
; In particular, they could use some fixnum arithmetic.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Help syntax and procedures; not exported.

(define-syntax u8->s8
  (syntax-rules ()
   ((_ octet0)
    (let ((octet octet0))
      (if (> octet 127)
          (- octet 256)
          octet)))))

(define-syntax s8->u8
  (syntax-rules ()
   ((_ val0)
    (let ((val val0))
      (if (negative? val)
          (+ val 256)
          val)))))

(define (make-uint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-uint-ref bytevector k endianness size)))

(define (make-sint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-sint-ref bytevector k endianness size)))

(define (make-uint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-uint-set! bytevector k n endianness size)))

(define (make-sint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-sint-set! bytevector k n endianness size)))

(define (make-ref/native base base-ref)
  (lambda (bytevector index)
    (ensure-aligned index base)
    (base-ref bytevector index (native-endianness))))

(define (make-set!/native base base-set!)
  (lambda (bytevector index val)
    (ensure-aligned index base)
    (base-set! bytevector index val (native-endianness))))

(define (ensure-aligned index base)
  (if (not (zero? (remainder index base)))
      (r6rs:error "non-aligned bytevector access" index base)))

(define (make-bytevector->int-list bytevector-u8-ref)
  (lambda (b endness size)
    (let ((ref (lambda (i) (bytevector-u8-ref b i endness size)))
	  (length (bytevector-length b)))
      (let loop ((i 0) (r '()))
	(if (>= i length)
	    (reverse r)
	    (loop (+ i size)
		  (cons (ref i) r)))))))

(define (make-int-list->bytevector bytevector-u8-set!)
  (lambda (l endness size)
    (let* ((bytevector (make-bytevector (* size (length l))))
	   (setter! (lambda (i n)
                      (bytevector-u8-set! bytevector i n endness size))))
      (let loop ((i 0) (l l))
	(if (null? l)
	    bytevector
	    (begin
	      (setter! i (car l))
	      (loop (+ i size) (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Exported procedures (and, alas, syntax).
;
; In Larceny, *any* symbol names a supported endianness.
; The symbols big and little have their expected meanings.
; All other symbols mean (native-endianness) with respect
; to integer operations, but mean the opposite of
; (native-endianness) with respect to IEEE-754 operations.
; For string operations, the endianness must be big or little.
;
; The extension described above is allowed by the current draft R6RS.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (export endianness native-endianness
;          bytevector? make-bytevector bytevector-length
;          bytevector-u8-ref bytevector-s8-ref
;          bytevector-u8-set! bytevector-s8-set!
;          bytevector-uint-ref bytevector-sint-ref
;          bytevector-uint-set! bytevector-sint-set!
;          bytevector-u16-ref bytevector-s16-ref
;          bytevector-u16-set! bytevector-s16-set!
;          bytevector-u16-native-ref bytevector-s16-native-ref
;          bytevector-u16-native-set! bytevector-s16-native-set!
;          bytevector-u32-ref bytevector-s32-ref
;          bytevector-u32-set! bytevector-s32-set!
;          bytevector-u32-native-ref bytevector-s32-native-ref
;          bytevector-u32-native-set! bytevector-s32-native-set!
;          bytevector-u64-ref bytevector-s64-ref
;          bytevector-u64-set! bytevector-s64-set!
;          bytevector-u64-native-ref bytevector-s64-native-ref
;          bytevector-u64-native-set! bytevector-s64-native-set!
;          bytevector=?
;          bytevector-ieee-single-native-ref bytevector-ieee-single-ref
;          bytevector-ieee-double-native-ref bytevector-ieee-double-ref
;          bytevector-ieee-single-native-set! bytevector-ieee-single-set!
;          bytevector-ieee-double-native-set! bytevector-ieee-double-set!
;          bytevector-copy! bytevector-copy
;          bytevector->u8-list u8-list->bytevector
;          bytevector->uint-list bytevector->sint-list
;          uint-list->bytevector sint-list->bytevector
;
;          utf8->string utf16->string utf32->string
;          string->utf8 string->utf16 string->utf32)

(define (native-endianness)
  'little)    ; correct for most modern processors

; These are provided by R7RS-small:
;
;          bytevector? make-bytevector bytevector-length
;          bytevector-u8-ref bytevector-u8-set! 

(define (bytevector-s8-ref b k)
  (u8->s8 (bytevector-u8-ref b k)))

(define (bytevector-s8-set! b k val)
  (bytevector-u8-set! b k (s8->u8 val)))

(define (bytevector-uint-ref bytevector index endness size)
  (case endness
   ((big)
    (do ((i 0 (+ i 1))
         (result 0 (+ (* 256 result)
                      (bytevector-u8-ref bytevector (+ index i)))))
        ((>= i size)
         result)))
   ((little)
    (do ((i (- size 1) (- i 1))
         (result 0 (+ (* 256 result)
                      (bytevector-u8-ref bytevector (+ index i)))))
        ((< i 0)
         result)))
   (else
    (bytevector-uint-ref bytevector index (native-endianness) size))))

(define (bytevector-sint-ref bytevector index endness size)
  (let* ((high-byte (bytevector-u8-ref bytevector
                               (if (eq? endness 'big)
                                   index
                                   (+ index size -1))))
         (uresult (bytevector-uint-ref bytevector index endness size)))
    (if (> high-byte 127)
        (- uresult (expt 256 size))
	uresult)))

; FIXME: Some of these procedures may not do enough range checking.

(define (bytevector-uint-set! bytevector index val endness size)
  (case endness
   ((little)
    (do ((i 0 (+ i 1))
         (val val (quotient val 256)))
        ((>= i size)
         (unspecified))
      (bytevector-u8-set! bytevector (+ index i) (remainder val 256))))
   ((big)
    (do ((i (- size 1) (- i 1))
         (val val (quotient val 256)))
        ((< i 0)
         (unspecified))
      (bytevector-u8-set! bytevector (+ index i) (remainder val 256))))
   (else
    (bytevector-uint-set! bytevector index val (native-endianness) size))))

(define (bytevector-sint-set! bytevector index val endness size)
  (let ((uval (if (< val 0)
                  (+ val (expt 256 size))
                  val)))
    (bytevector-uint-set! bytevector index uval endness size)))
  
(define bytevector-u16-ref (make-uint-ref 2))
(define bytevector-u16-set! (make-uint-set! 2))
(define bytevector-s16-ref (make-sint-ref 2))
(define bytevector-s16-set! (make-sint-set! 2))
(define bytevector-u16-native-ref (make-ref/native 2 bytevector-u16-ref))
(define bytevector-u16-native-set! (make-set!/native 2 bytevector-u16-set!))
(define bytevector-s16-native-ref (make-ref/native 2 bytevector-s16-ref))
(define bytevector-s16-native-set! (make-set!/native 2 bytevector-s16-set!))

(define bytevector-u32-ref (make-uint-ref 4))
(define bytevector-u32-set! (make-uint-set! 4))
(define bytevector-s32-ref (make-sint-ref 4))
(define bytevector-s32-set! (make-sint-set! 4))
(define bytevector-u32-native-ref (make-ref/native 4 bytevector-u32-ref))
(define bytevector-u32-native-set! (make-set!/native 4 bytevector-u32-set!))
(define bytevector-s32-native-ref (make-ref/native 4 bytevector-s32-ref))
(define bytevector-s32-native-set! (make-set!/native 4 bytevector-s32-set!))

(define bytevector-u64-ref (make-uint-ref 8))
(define bytevector-u64-set! (make-uint-set! 8))
(define bytevector-s64-ref (make-sint-ref 8))
(define bytevector-s64-set! (make-sint-set! 8))
(define bytevector-u64-native-ref (make-ref/native 8 bytevector-u64-ref))
(define bytevector-u64-native-set! (make-set!/native 8 bytevector-u64-set!))
(define bytevector-s64-native-ref (make-ref/native 8 bytevector-s64-ref))
(define bytevector-s64-native-set! (make-set!/native 8 bytevector-s64-set!))

(define (bytevector=? b1 b2)
  (if (or (not (bytevector? b1))
          (not (bytevector? b2)))
      (r6rs:error 'bytevector=? "Illegal arguments: " b1 b2)
      (let ((n1 (bytevector-length b1))
            (n2 (bytevector-length b2)))
        (and (= n1 n2)
             (do ((i 0 (+ i 1)))
                 ((or (= i n1)
                      (not (= (bytevector-u8-ref b1 i)
                              (bytevector-u8-ref b2 i))))
                  (= i n1)))))))

; FIXME: should use word-at-a-time when possible

(define (bytevector-fill! b fill)
  (if (<= -128 fill -1)
      (bytevector-fill! b (+ fill 256))
      (let ((n (bytevector-length b)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (bytevector-u8-set! b i fill)))))

(define (r6rs:bytevector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
      (do ((i 0 (+ i 1)))
          ((>= i count))
        (bytevector-u8-set! target
                       (+ target-start i)
                       (bytevector-u8-ref source (+ source-start i))))
      (do ((i (- count 1) (- i 1)))
          ((< i 0))
        (bytevector-u8-set! target
                       (+ target-start i)
                       (bytevector-u8-ref source (+ source-start i))))))

(define (bytevector->u8-list b)
  (let ((n (bytevector-length b)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (bytevector-u8-ref b i) result)))
        ((< i 0)
         result))))

(define (bytevector->s8-list b)
  (let ((n (bytevector-length b)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (bytevector-s8-ref b i) result)))
        ((< i 0)
         result))))

(define (u8-list->bytevector vals)
  (let* ((n (length vals))
         (b (make-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
        ((null? vals))
      (bytevector-u8-set! b i (car vals)))
    b))

(define (s8-list->bytevector vals)
  (let* ((n (length vals))
         (b (make-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
        ((null? vals))
      (bytevector-s8-set! b i (car vals)))
    b))

(define bytevector->uint-list (make-bytevector->int-list bytevector-uint-ref))
(define bytevector->sint-list (make-bytevector->int-list bytevector-sint-ref))

(define uint-list->bytevector (make-int-list->bytevector bytevector-uint-set!))
(define sint-list->bytevector (make-int-list->bytevector bytevector-sint-set!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Bytevector operations that involve IEEE floating point.
;
; Only the generalized procedures are supplied here;
; the -native- procedures are in ieee-impl.scm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; The exported procedures

(define (bytevector-ieee-single-ref bytevector k endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (remainder k 4))
          (bytevector-ieee-single-native-ref bytevector k)
          (let ((b (make-bytevector 4)))
            (r6rs:bytevector-copy! bytevector k b 0 4)
            (bytevector-ieee-single-native-ref b 0)))
      (let ((b (make-bytevector 4)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-single-native-ref b 0))))

(define (bytevector-ieee-double-ref bytevector k endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (remainder k 8))
          (bytevector-ieee-double-native-ref bytevector k)
          (let ((b (make-bytevector 8)))
            (r6rs:bytevector-copy! bytevector k b 0 8)
            (bytevector-ieee-double-native-ref b 0)))
      (let ((b (make-bytevector 8)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 7)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 6)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 5)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector (+ k 4)))
        (bytevector-u8-set! b 4 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 5 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 6 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 7 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-double-native-ref b 0))))

(define (bytevector-ieee-single-set! bytevector k x endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (remainder k 4))
          (bytevector-ieee-single-native-set! bytevector k x)
          (let ((b (make-bytevector 4)))
            (bytevector-ieee-single-native-set! b 0 x)
            (r6rs:bytevector-copy! b 0 bytevector k 4)))
      (let ((b (make-bytevector 4)))
        (bytevector-ieee-single-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 0)))))

(define (bytevector-ieee-double-set! bytevector k x endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (remainder k 8))
          (bytevector-ieee-double-native-set! bytevector k x)
          (let ((b (make-bytevector 8)))
            (bytevector-ieee-double-native-set! b 0 x)
            (r6rs:bytevector-copy! b 0 bytevector k 8)))
      (let ((b (make-bytevector 8)))
        (bytevector-ieee-double-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 7))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 6))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 5))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 4))
        (bytevector-u8-set! bytevector (+ k 4) (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 5) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 6) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 7) (bytevector-u8-ref b 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversions between bytevectors and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (utf-16-codec) might write a byte order mark,
; so it's better not to use textual i/o for this.

(define (string->utf16 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((not (null? (cdr rest)))
                            (apply assertion-violation 'string->utf16
                                   "too many arguments" string rest))
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (endianness-violation
                                  'string->utf16
                                  (car rest)))))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (n (string-length string)))

    (define (result-length)
      (do ((i 0 (+ i 1))
           (k 0 (let ((sv (char->integer (string-ref string i))))
                  (if (< sv #x10000) (+ k 2) (+ k 4)))))
          ((= i n) k)))

    (let ((bv (make-bytevector (result-length))))

      (define (loop i k)
        (if (< i n)
            (let ((sv (char->integer (string-ref string i))))
              (if (< sv #x10000)
                  (let ((hibits (bit-field sv 8 16))
                        (lobits (bit-field sv 0 8)))
                    (bytevector-u8-set! bv (+ k hi) hibits)
                    (bytevector-u8-set! bv (+ k lo) lobits)
                    (loop (+ i 1) (+ k 2)))
                  (let* ((x (- sv #x10000))
                         (hibits (bit-field x 10 20))
                         (lobits (bit-field x 0 10))
                         (hi16 (bitwise-ior #xd800 hibits))
                         (lo16 (bitwise-ior #xdc00 lobits))
                         (hi1 (bit-field hi16 8 16))
                         (lo1 (bit-field hi16 0 8))
                         (hi2 (bit-field lo16 8 16))
                         (lo2 (bit-field lo16 0 8)))
                    (bytevector-u8-set! bv (+ k hi) hi1)
                    (bytevector-u8-set! bv (+ k lo) lo1)
                    (bytevector-u8-set! bv (+ k hi 2) hi2)
                    (bytevector-u8-set! bv (+ k lo 2) lo2)
                    (loop (+ i 1) (+ k 4)))))))

      (loop 0 0)
      bv)))

; The second argument should be optional should be optional,
; and was optional in the R5.94RS draft, but was made mandatory
; in the R5.95RS draft by someone who misinterpreted John Cowan's
; response of 27 May 2007 to an ambiguous question posed by
; Mike Sperber.  This error was not spotted by anyone, and
; made its way into the ratified R6RS.
;
; Larceny will not perpetuate this error.  In Larceny, the
; second argument is optional.
;
; The R6RS also contradicts itself by saying the bytevector
; will be decoded according to UTF-16BE or UTF-16LE, which
; implies any BOM must be ignored.  I believe the intended
; specification was along these lines:
;
;    Bytevector is decoded acccording to UTF-16, UTF-16BE,
;    UTF-16LE, or a fourth encoding scheme that differs from
;    all three of those, depending upon the optional arguments
;    endianness and endianness-mandatory.  If endianness
;    is the symbol big and endianness-mandatory is absent
;    or false, then bytevector is decoded according to
;    UTF-16.  If endianness is the symbol big and
;    endianness-mandatory is #t, then bytevector is decoded
;    according to UTF-16BE.  If endianness is the symbol
;    little and endianness-mandatory is #t, then bytevector
;    is decoded according to UTF-16LE.  If endianness is
;    the symbol little and endianness-mandatory is absent
;    or #f, then the bytevector is decoded according to
;    UTF-16 if it begins with a BOM but is decoded according
;    to UTF-16LE if it does not begin with a BOM; note that
;    this fourth decoding does not correspond to any of the
;    seven Unicode encoding schemes that are defined by the
;    Unicode standard.
;
; That is the specification implemented by Larceny.

(define (utf16->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
          (and (<= 2 n)
               (let ((b0 (bytevector-u8-ref bytevector 0))
                     (b1 (bytevector-u8-ref bytevector 1)))
                 (or (and (= b0 #xfe) (= b1 #xff) 'big)
                     (and (= b0 #xff) (= b1 #xfe) 'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                            (apply assertion-violation 'utf16->string
                                   "illegal arguments" bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                                'big
                                (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                                'little
                                (or begins-with-bom? 'little)))
                           (else (endianness-violation
                                  'utf16->string
                                  (car rest)))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (replacement-character (integer->char #xfffd)))

    ; computes the length of the encoded string

    (define (result-length)
      (define (loop i k)
        (if (>= i n)
            k
            (let ((octet (bytevector-u8-ref bytevector i)))
              (cond ((< octet #xd8)
                     (loop (+ i 2) (+ k 1)))
                    ((< octet #xdc)
                     (let* ((i2 (+ i 2))
                            (octet2 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i2)
                                        0)))
                       (if (<= #xdc octet2 #xdf)
                           (loop (+ i 4) (+ k 1))
                           ; bad surrogate pair, becomes replacement character
                           (loop i2 (+ k 1)))))
                    (else (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
          (loop (+ hi 2) 0)
          (loop hi 0)))

    (if (odd? n)
        (assertion-violation 'utf16->string
                             "bytevector has odd length" bytevector))

    (let ((s (make-string (result-length))))
      (define (loop i k)
        (if (< i n)
            (let ((hibits (bytevector-u8-ref bytevector (+ i hi)))
                  (lobits (bytevector-u8-ref bytevector (+ i lo))))
              (cond ((< hibits #xd8)
                     (let ((c (integer->char
                               (bitwise-ior
                                (arithmetic-shift hibits 8)
                                lobits))))
                       (string-set! s k c))
                     (loop (+ i 2) (+ k 1)))
                    ((< hibits #xdc)
                     (let* ((i2 (+ i hi 2))
                            (i3 (+ i lo 2))
                            (octet2 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i2)
                                        0))
                            (octet3 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i3)
                                        0)))
                       (if (<= #xdc octet2 #xdf)
                           (let* ((sv (+ #x10000
                                         (arithmetic-shift
                                          (bitwise-and
                                           (bitwise-ior
                                            (arithmetic-shift
                                             hibits 8)
                                            lobits)
                                           #x03ff)
                                          10)
                                         (bitwise-and
                                          (bitwise-ior
                                           (arithmetic-shift
                                            octet2 8)
                                           octet3)
                                          #x03ff)))
                                  (c (if (<= #x10000 sv #x10ffff)
                                         (integer->char sv)
                                         replacement-character)))
                             (string-set! s k c)
                             (loop (+ i 4) (+ k 1)))
                           ; bad surrogate pair
                           (begin (string-set! s k replacement-character)
                                  (loop (+ i 2) (+ k 1))))))
                    ((< hibits #xe0)
                     ; second surrogate not preceded by a first surrogate
                     (string-set! s k replacement-character)
                     (loop (+ i 2) (+ k 1)))
                    (else
                     (let ((c (integer->char
                               (bitwise-ior
                                (arithmetic-shift hibits 8)
                                lobits))))
                       (string-set! s k c))
                     (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
          (loop 2 0)
          (loop 0 0))
      s)))

; There is no utf-32-codec, so we can't use textual i/o for this.

(define (string->utf32 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (endianness-violation
                                  'string->utf32
                                  (car rest)))))
         (n (string-length string))
         (result (make-bytevector (* 4 n))))
    (do ((i 0 (+ i 1)))
        ((= i n) result)
      (bytevector-u32-set! result
                           (* 4 i)
                           (char->integer (string-ref string i))
                           endianness))))

; There is no utf-32-codec, so we can't use textual i/o for this.

(define (utf32->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
          (and (<= 4 n)
               (let ((b0 (bytevector-u8-ref bytevector 0))
                     (b1 (bytevector-u8-ref bytevector 1))
                     (b2 (bytevector-u8-ref bytevector 2))
                     (b3 (bytevector-u8-ref bytevector 3)))
                 (or (and (= b0 0) (= b1 0) (= b2 #xfe) (= b3 #xff)
                          'big)
                     (and (= b0 #xff) (= b1 #xfe) (= b2 0) (= b3 0)
                          'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                            (apply assertion-violation 'utf32->string
                                   "illegal arguments" bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                                'big
                                (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                                'little
                                (or begins-with-bom? 'little)))
                           (else (endianness-violation
                                  'utf32->string
                                  (car rest)))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         (i0 (if begins-with-bom? 4 0))

         (result (if (zero? (remainder n 4))
                     (make-string (quotient (- n i0) 4))
                     (assertion-violation
                      'utf32->string
                      "Bytevector has bad length." bytevector))))

    (do ((i i0 (+ i 4))
         (j 0 (+ j 1)))
        ((= i n) result)
      (let* ((sv (bytevector-u32-ref bytevector i endianness))
             (sv (cond ((< sv #xd800) sv)
                       ((< sv #xe000) #xfffd) ; replacement character
                       ((< sv #x110000) sv)
                       (else #xfffd)))        ; replacement character
             (c (integer->char sv)))
        (string-set! result j c)))))

(define (endianness-violation who what)
  (assertion-violation who "bad endianness" what))
