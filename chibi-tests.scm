;;;; Chibi tests for s16vectors (if one vector type works, they all work)
(import (scheme base))
(import (scheme write))
(import (chibi test))
(import (srfi 128))
(import (srfi 160 s16))

(begin
  (define (sub1 x) (- x 1)))
(include "shared-tests.scm")
