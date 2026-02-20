#!/usr/bin/env chibi-scheme
;;;; Test suite for email address validator
;;;; SPDX-License-Identifier: MIT
;;;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler

(import (scheme base)
        (scheme write)
        (scheme cxr)
        (srfi 64)
        (validate))

(include "test-data.scm")

(test-begin "email-validation")

(for-each
 (lambda (test-case)
   (let ((test-id (car test-case))
         (address (cadr test-case))
         (expected (caddr test-case)))
     (test-equal
      (string-append "Test #" (number->string test-id) ": " address)
      expected
      (valid-email-address? address))))
 email-test-suite)

(test-end "email-validation")