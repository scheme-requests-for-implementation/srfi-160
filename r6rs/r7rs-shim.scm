;;;; R7RS bytevector procedures that aren't primitive

;; From Chibi Scheme
(define (bytevector-copy! to at from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (bytevector-length from)))
         (limit (min end (+ start (- (bytevector-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (bytevector-u8-set! to i (bytevector-u8-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (bytevector-u8-set! to i (bytevector-u8-ref from j))))))
 
(define (bytevector-copy from)
  (let ((to (make-bytevector (bytevector-length from))))
     (bytevector-copy! to 0 from)
     to))

;; define R6RS bytevector-copy! in terms of R7RS version
(define (r6rs:bytevector-copy! source source-start target target-start count)
  (bytevector-copy! target target-start source source-start (+ source-start count)))


;; From Larceny

(define (string->utf8 string . rest)
  (let* ((n (string-length string))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (k (do ((i start (+ i 1))
                 (k 0 (+ k (let ((sv (char->integer (string-ref string i))))
                             (cond ((<= sv #x007f) 1)
                                   ((<= sv #x07ff) 2)
                                   ((<= sv #xffff) 3)
                                   (else 4))))))
                ((= i end) k)))
         (bv (make-bytevector k)))
    (define (loop i j)
      (if (>= i end)
          bv
          (let ((sv (char->integer (string-ref string i))))
            (cond ((<= sv #x007f)
                   (bytevector-u8-set! bv j sv)
                   (loop (+ i 1) (+ j 1)))
                  ((<= sv #x07ff)
                   (let ((u0 (bitwise-ior #b11000000
                                          (bit-field sv 6 11)))
                         (u1 (bitwise-ior #b10000000
                                          (bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (loop (+ i 1) (+ j 2))))
                  ((<= sv #xffff)
                   (let ((u0 (bitwise-ior #b11100000
                                          (bit-field sv 12 16)))
                         (u1 (bitwise-ior #b10000000
                                          (bit-field sv 6 12)))
                         (u2 (bitwise-ior #b10000000
                                          (bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (bytevector-u8-set! bv (+ j 2) u2)
                     (loop (+ i 1) (+ j 3))))
                  (else
                   (let ((u0 (bitwise-ior #b11110000
                                          (bit-field sv 18 21)))
                         (u1 (bitwise-ior #b10000000
                                          (bit-field sv 12 18)))
                         (u2 (bitwise-ior #b10000000
                                          (bit-field sv 6 12)))
                         (u3 (bitwise-ior #b10000000
                                          (bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (bytevector-u8-set! bv (+ j 2) u2)
                     (bytevector-u8-set! bv (+ j 3) u3)
                     (loop (+ i 1) (+ j 4))))))))
    (loop start 0)))

; Given a bytevector containing the UTF-8 encoding
; of a string, decodes and returns a newly allocated
; string (unless empty).
;
; If the bytevector begins with the three-byte sequence
; #xef #xbb #xbf, then those bytes are ignored.  (They
; are conventionally used as a signature to indicate
; UTF-8 encoding.  The string->utf8 procedure does not
; emit those bytes, but UTF-8 encodings produced by
; other sources may contain them.)
;
; The main difficulty is that Unicode Corrigendum #1
; ( http://unicode.org/versions/corrigendum1.html )
; forbids interpretation of illegal code unit sequences,
; which include non-shortest forms.  A UTF-8 decoder
; must therefore detect non-shortest forms and treat
; them as errors.
;
; Another difficulty is that the specification of this
; particular decoder says it will replace an illegal
; code unit sequence by a replacement character, but
; does not fully specify the recovery process, which
; affects the number of replacement characters that
; will appear in the result.
;
; Ignoring the special treatment of a ZERO WIDTH
; NO-BREAK SPACE at the beginning of a bytevector,
; the decoding is implemented by the following
; state machine.  q0 is the start state and the
; only state in which no more input is acceptable.
;
; q0 --- dispatching on the first byte of a character
; Dispatch on the next byte according to Table 3.1B
; of Corrigendum #1.  Note that there are two error
; ranges not shown in that table, for a total of 9.
; The 00..7f, 80..c1, and f5..ff ranges remain in
; state q0.  00..7f is an Ascii character; the other
; two ranges that remain in state q0 are illegal.
;
; q1 --- expecting one more byte in range 80..bf
;
; q2 --- expecting two more bytes, the first in range lower..bf
;
; q3 --- expecting three more bytes, the first in range lower..upper

(define (utf8->string bv . rest)
  (let* ((n (bytevector-length bv))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (replacement-character (integer->char #xfffd))
         (begins-with-bom?
          (and (<= 3 n)
               (= #xef (bytevector-u8-ref bv 0))
               (= #xbb (bytevector-u8-ref bv 1))
               (= #xbf (bytevector-u8-ref bv 2)))))

    (define bits->char (lambda (bits)
                         (cond ((<= 0 bits #xd7ff)
                                (integer->char bits))
                               ((<= #xe000 bits #x10ffff)
                                (integer->char bits))
                               (else
                                replacement-character))))

    (define (result-length)
      ; i is index of the next byte
      ; k is the number of characters encoded by bytes 0 through i-1
      (define (q0 i k)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((<= unit #x7f)
                     (q0 i1 k1))
                    ((<= unit #xc1)
                     ; illegal
                     (q0 i1 k1))
                    ((<= unit #xdf)
                     (q1 i1 k1))
                    ((<= unit #xe0)
                     (q2 i1 k1 #xa0))
                    ((<= unit #xef)
                     (q2 i1 k1 #x80))
                    ((<= unit #xf0)
                     (q3 i1 k1 #x90 #xbf))
                    ((<= unit #xf3)
                     (q3 i1 k1 #x80 #xbf))
                    ((<= unit #xf4)
                     (q3 i1 k1 #x80 #x8f))
                    (else
                     ; illegal
                     (q0 i1 k1))))))
      (define (q1 i k)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit #x80)
                     ; illegal
                     (q0 i k))
                    ((<= unit #xbf)
                     (q0 i1 k))
                    (else
                     ; illegal
                     (q0 i k))))))
      (define (q2 i k lower)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (q0 i k))
                    ((<= unit #xbf)
                     (q1 i1 k))
                    (else
                     ; illegal
                     (q0 i k))))))
      (define (q3 i k lower upper)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (q0 i k))
                    ((<= unit upper)
                     (q2 i1 k #x80))
                    (else
                     ; illegal
                     (q0 i k))))))
      (if (and begins-with-bom? (= start 0))
          (q0 3 0)
          (q0 start 0)))

    (let* ((k (result-length))
           (s (make-string k)))

      ; i is index of the next byte in bv
      ; k is index of the next character in s

      (define (q0 i k)
        (if (< i end)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((<= unit #x7f)
                     (string-set! s k (integer->char unit))
                     (q0 i1 k1))
                    ((<= unit #xc1)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i1 k1))
                    ((<= unit #xdf)
                     (q1 i1 k (bitwise-and unit #x1f)))
                    ((<= unit #xe0)
                     (q2 i1 k #xa0 0))
                    ((<= unit #xef)
                     (q2 i1 k #x80 (bitwise-and unit #x0f)))
                    ((<= unit #xf0)
                     (q3 i1 k #x90 #xbf 0))
                    ((<= unit #xf3)
                     (q3 i1 k #x80 #xbf (bitwise-and unit #x07)))
                    ((<= unit #xf4)
                     (q3 i1 k #x80 #x8f (bitwise-and unit #x07)))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i1 k1))))))
      (define (q1 i k bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((< unit #x80)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i k1))
                    ((<= unit #xbf)
                     (string-set! s k (bits->char
                                       (bitwise-ior
                                        (arithmetic-shift bits 6)
                                        (bitwise-and unit #x3f))))
                     (q0 i1 k1))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i k1))))))
      (define (q2 i k lower bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))
                    ((<= unit #x00bf)
                     (q1 i1 k (bitwise-ior
                               (arithmetic-shift bits 6)
                               (bitwise-and unit #x3f))))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))))))
      (define (q3 i k lower upper bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))
                    ((<= unit upper)
                     (q2 i1 k #x80 (bitwise-ior
                                    (arithmetic-shift bits 6)
                                    (bitwise-and unit #x3f))))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))))))
      (if (and begins-with-bom? (= start 0))
          (q0 3 0)
          (q0 start 0))
      s)))

;; From Larceny, not currently used by this package

(define (bytevector . args)
  (let* ((n (length args))
         (bv (make-bytevector n)))
    (do ((i 0 (+ i 1))
         (args args (cdr args)))
        ((= i n)
         bv)
      (bytevector-u8-set! bv i (car args)))))
 
(define (bytevector-append . args)
  (let* ((lengths (map bytevector-length args))
         (n (apply + lengths))
         (bv (make-bytevector n)))
    (do ((j 0 (+ j (car lengths)))
         (args args (cdr args))
         (lengths lengths (cdr lengths)))
        ((null? args) bv)
      (bytevector-copy! bv j (car args)))))
 

