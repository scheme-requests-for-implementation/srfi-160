;; The representation of bit and complex vectors

(define-record-type <u1vector> (raw-make-u1vector len bv) u1vector?
  (len u1vector-length)
  (bv bv1))

(define-record-type <c64vector> (raw-make-c64vector bv) c64vector?
  (bv bv64))

(define-record-type <c128vector> (raw-make-c128vector bv) c128vector?
  (bv bv128))

