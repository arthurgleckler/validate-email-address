# SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
# SPDX-License-Identifier: MIT

.PHONY: check check-chibi check-gauche clean

check: check-chibi check-gauche

check-chibi:
	@echo "Running tests with Chibi Scheme."
	@chibi-scheme run-tests.scm

check-gauche:
	@echo "Running tests with Gauche Scheme."
	@gosh -I. run-tests.scm