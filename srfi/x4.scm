(library (srfi x4)
  (export
    make-u1vector make-c64vector make-c128vector 
    u1vector c64vector c128vector 
    u1vector?  c64vector?  c128vector?  
    u1vector-length c64vector-length c128vector-length 
    u1vector-ref c64vector-ref c128vector-ref 
    u1vector-set!  c64vector-set!  c128vector-set!  
    u1vector->list c64vector->list c128vector->list 
    list->u1vector list->c64vector list->c128vector)

  (import (rnrs base)
          (rnrs records syntactic)
          (rename (only (rnrs arithmetic bitwise)
                        bitwise-and bitwise-ior bitwise-not
                        bitwise-arithmetic-shift)
            (bitwise-arithmetic-shift arithmetic-shift))
          (only (rnrs r5rs) quotient remainder)
          (srfi :4)
          (include))

  (include "srfi/x4/r6rec.scm")
  (include "srfi/x4/bit.scm")
  (include "srfi/x4/complex.scm")
)
