;; The representation of bit and complex vectors

(define-record-type (<u1vector> raw-make-u1vector u1vector?)
  (fields (immutable len u1vector-length)
          (immutable bv bv1)))

(define-record-type (<c64vector> raw-make-c64vector c64vector?)
  (fields (immutable bv bv64)))

(define-record-type (<c128vector> raw-make-c128vector c128vector?)
  (fields (immutable bv bv128)))

