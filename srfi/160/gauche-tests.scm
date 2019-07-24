;; To run, from the top source directory:
;;  gosh -I. srfi/160/gauche-tests.scm
;; Requires gauche newer than 7/25/2019 commit (#53b950d)
(use gauche.test)

(test-start "srfi-160.s16 tests")

(define-module srfi-160-test
  (use compat.chibi-test)
  (use srfi.160.s16)
  (chibi-test
   (include "chibi-tests.scm")))

(test-end)
