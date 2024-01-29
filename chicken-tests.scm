;;; SPDX-FileCopyrightText: 2018 John Cowan <cowan@ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;;; Chicken Tests for s16vectors (if one vector type works, they all work)
(import (scheme))
(import (test))
(import (srfi 128))
(import (srfi 160 s16))
(include "shared-tests.scm")

