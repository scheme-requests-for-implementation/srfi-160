;;;; Basic tests of (r6rs bytevectors).

(import (rnrs base)
        (r6rs bytevectors)
        (rnrs io simple))

;; For string tests
(when (defined? 'setlocale)
  (setlocale LC_ALL "C"))

(include "shared-tests.scm")
