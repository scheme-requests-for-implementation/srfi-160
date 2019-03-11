;; The representation of complex vectors

(define-record-type (<c64vector> raw-make-c64vector c64vector?)
  (fields (immutable bv bv64)))

(define-record-type (<c128vector> raw-make-c128vector c128vector?)
  (fields (immutable bv bv128)))

