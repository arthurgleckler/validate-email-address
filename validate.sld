;;;; SPDX-License-Identifier: MIT
;;;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;;;
;;;; Strict binary email address validator for user input (R7RS library)

(define-library (validate)
  (export valid-email-address?)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (chibi parse)
          (chibi string))
  (include "validate.scm"))