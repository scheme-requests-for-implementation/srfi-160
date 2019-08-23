(use gauche.test)
(use compat.chibi-test)
(add-load-path "." :relative)
(use srfi.160.base)
(use srfi.160.s16)

(test-start "srfi-160")

(chibi-test
 (include "shared-base-tests.scm")
 (include "shared-tests.scm"))

(test-end)

