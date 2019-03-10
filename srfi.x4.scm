(module (srfi x4) ()
  (import scheme)
  (import (only (chicken base) include))
  (import (only (chicken module) export))
  (import (only (chicken bitwise) bitwise-and bitwise-ior
                                  bitwise-not arithmetic-shift))
  (import (srfi 4))
  (import (srfi 9))

  (export
    make-u1vector make-c64vector make-c128vector 
    u1vector c64vector c128vector 
    u1vector?  c64vector?  c128vector?  
    u1vector-length c64vector-length c128vector-length 
    u1vector-ref c64vector-ref c128vector-ref 
    u1vector-set!  c64vector-set!  c128vector-set!  
    u1vector->list c64vector->list c128vector->list 
    list->u1vector list->c64vector list->c128vector)

  (include "srfi/x4/r7rec.scm")
  (include "srfi/x4/complex.scm")
  (include "srfi/x4/bit.scm")
)
