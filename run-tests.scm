#!/usr/bin/env chibi-scheme
;;;; Test runner for strict binary email validation
;;;; SPDX-License-Identifier: MIT
;;;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;;;
;;;; Run all tests.

(import (scheme base)
        (scheme write)
        (scheme cxr)
        (validate))

(include "test-data.scm")

(define (run-test test-id address expected)
  (let ((result (valid-email-address? address)))
    (if (eq? result expected)
        'pass
        'fail)))

(define (run-all-tests tests)
  (let loop ((remaining tests)
             (passed 0)
             (failed 0)
             (failures '()))
    (if (null? remaining)
        (list passed failed (reverse failures))
        (let* ((test (car remaining))
               (test-id (car test))
               (address (cadr test))
               (expected (caddr test))
               (result (run-test test-id address expected)))
          (if (eq? result 'pass)
              (loop (cdr remaining) (+ passed 1) failed failures)
              (loop (cdr remaining) passed (+ failed 1)
                    (cons (list test-id
				address
				expected
				result)
                          failures)))))))

(define (main)
  (display "Running strict binary email validation tests...\n")
  (display "Total tests: ")
  (display (length email-test-suite))
  (newline)
  (newline)
  (let ((results (run-all-tests email-test-suite)))
    (let ((passed (car results))
          (failed (cadr results))
          (failures (caddr results)))
      (display "Results:\n")
      (display "  Passed: ")
      (display passed)
      (display " / ")
      (display (length email-test-suite))
      (display " (")
      (display (inexact (/ (* passed 100.0) (length email-test-suite))))
      (display "%)\n")
      (display "  Failed: ")
      (display failed)
      (newline)
      (newline)
      (when (> failed 0)
        (display "Failed tests:\n")
        (for-each
         (lambda (failure)
           (let ((test-id (car failure))
                 (address (cadr failure))
                 (expected (caddr failure))
                 (actual (cadddr failure)))
             (display "  Test #")
             (display test-id)
             (display ": ")
             (write address)
             (display " - Expected: ")
             (display (if expected "accept" "reject"))
             (display ", Got: ")
             (display (if actual "accept" "reject"))
             (newline)))
         failures)))))

(main)