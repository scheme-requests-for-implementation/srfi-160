;; The representation of complex vectors

(define-record-type <c64vector> (raw-make-c64vector bv) c64vector?
  (bv bv64))

(define-record-type <c128vector> (raw-make-c128vector bv) c128vector?
  (bv bv128))

