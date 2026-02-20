;;;; SPDX-License-Identifier: MIT
;;;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;;;
;;;; Strict binary email address validator for user input (R7RS library)
;;;;
;;;; This library works with both Chibi Scheme and Gauche Scheme.

(define-library (validate)
  (export valid-email-address?)
  (import (scheme base)
          (scheme char))
  (cond-expand
   (chibi
    (import (scheme cxr)
            (chibi parse)
            (chibi string))
    (include "validate-chibi.scm"))
   (gauche
    (import (gauche base)
            (parser peg)
            (srfi 13))
    (include "validate-gauche.scm"))
   (else
    (begin
      (error "This library requires either Chibi Scheme or Gauche Scheme")))))