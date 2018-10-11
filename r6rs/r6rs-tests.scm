;;;; Basic tests of (rnrs bytevectors).

(import (rnrs base)
        (rename (rnrs bytevectors) (bytevector-copy! r6rs:bytevector-copy!))
        (rnrs io simple))

(include "shared-tests.scm")
