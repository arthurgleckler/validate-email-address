;;;; Strict binary email address validator for user input
;;;; for Gauche Scheme
;;;;
;;;; SPDX-License-Identifier: MIT
;;;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;;;
;;;; Return #true (valid) or #false (invalid).
;;;; Rejects control characters, including line breaks (CR, LF, CRLF).

;; CFWS = Comments and Folding White Space
;; FWS = Folding White Space

;; Character predicates for RFC 5322
(define (char-atext? character)
  (or (char-alphabetic? character)
      (char-numeric? character)
      (memv character '(#\! #\# #\$ #\% #\& #\' #\* #\+ #\- #\/ #\= #\?
		 #\^ #\_ #\` #\{ #\| #\} #\~))))

(define (char-dtext? character)
  ;; dtext = %d33-90 / %d94-126 (printable US-ASCII except [\])
  (and (char? character)
       (let ((n (char->integer character)))
	 (or (and (>= n 33) (<= n 90))
	     (and (>= n 94) (<= n 126))))))

(define (char-qtext? character)
  ;; qtext = %d33 / %d35-91 / %d93-126 (printable except " and \)
  (and (char? character)
       (let ((n (char->integer character)))
	 (or (= n 33)
	     (and (>= n 35) (<= n 91))
	     (and (>= n 93) (<= n 126))))))

(define (char-ctext? character)
  ;; ctext = %d33-39 / %d42-91 / %d93-126 (printable except ( ) \)
  (and (char? character)
       (let ((n (char->integer character)))
	 (or (and (>= n 33) (<= n 39))
	     (and (>= n 42) (<= n 91))
	     (and (>= n 93) (<= n 126))))))

(define (char-wsp? character)
  ;; WSP = SP / HTAB
  (or (eqv? character #\space) (eqv? character #\tab)))

(define (char-vchar? character)
  ;; VCHAR = %d33-126 (visible printing characters)
  (and (char? character)
       (let ((n (char->integer character)))
	 (and (>= n 33) (<= n 126)))))

;; Check for control characters (strict mode for user input)
(define (has-control-chars? string)
  ;; Check for actual control characters (0-31, 127)
  (let ((length (string-length string)))
    (let loop ((i 0))
      (and (< i length)
	   (let ((n (char->integer (string-ref string i))))
	     (or (< n 32)			; C0 control characters
		 (= n 127)			; DEL
		 (loop (+ i 1))))))))

(define (char-class pred label)
  ($satisfy pred label))

;; FWS (Folding White Space) - simplified, no CRLF allowed in strict mode
(define parse-fws
  ($many1 (char-class char-wsp? 'wsp)))

;; quoted-pair = "\" (VCHAR / WSP)
(define parse-quoted-pair
  ($seq ($char #\\)
	($or (char-class char-vchar? 'vchar)
	     (char-class char-wsp? 'wsp))))

;; comment (recursive) - simplified
(define parse-comment
  ($lazy
   ($seq ($char #\()
	 ($many ($or (char-class char-ctext? 'ctext)
		     (char-class char-wsp? 'wsp)
		     parse-quoted-pair
		     parse-comment))
	 ($char #\)))))

;; CFWS = (1*([FWS] comment) [FWS]) / FWS
;; Use $try to allow backtracking if optional FWS matches but no comment follows
(define parse-cfws
  ($or
   ($try ($seq
	  ($many1
	   ($seq ($optional parse-fws)
		 parse-comment))
	  ($optional parse-fws)))
   parse-fws))

;; atom = [CFWS] 1*atext [CFWS]
(define parse-atom
  ($seq
   ($optional parse-cfws)
   ($many1 (char-class char-atext? 'atext))
   ($optional parse-cfws)))

;; dot-atom-text = 1*atext *("." 1*atext)
(define parse-dot-atom-text
  ($seq
   ($many1 (char-class char-atext? 'atext))
   ($many
    ($seq ($char #\.)
	  ($many1 (char-class char-atext? 'atext))))))

;; dot-atom = [CFWS] dot-atom-text [CFWS]
(define parse-dot-atom
  ($seq
   ($optional parse-cfws)
   parse-dot-atom-text
   ($optional parse-cfws)))

;; qcontent = qtext / quoted-pair
(define parse-qcontent
  ($or
   (char-class char-qtext? 'qtext)
   parse-quoted-pair))

;; quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
(define parse-quoted-string
  ($seq
   ($optional parse-cfws)
   ($char #\")
   ($many
    ($seq ($optional parse-fws)
	  parse-qcontent))
   ($optional parse-fws)
   ($char #\")
   ($optional parse-cfws)))

;; word = atom / quoted-string
(define parse-word
  ($or parse-atom parse-quoted-string))

;; obs-local-part = word *("." word)
;; In practice, CFWS can appear around dots in obsolete syntax
;; Use $try to enable backtracking if CFWS consumed but no dot follows
(define parse-obs-local-part
  ($seq
   parse-word
   ($many
    ($try ($seq ($optional parse-cfws) ($char #\.) ($optional parse-cfws)
		parse-word)))))

;; local-part = dot-atom / quoted-string / obs-local-part
;; Due to Gauche PEG limitations with backtracking across alternatives,
;; we use only obs-local-part which is most general and matches all valid forms
(define parse-local-part
  parse-obs-local-part)

;; dtext-no-obs = %d33-90 / %d94-126
(define parse-dtext
  (char-class char-dtext? 'dtext))

;; dcontent = dtext / quoted-pair
(define parse-dcontent
  ($or parse-dtext parse-quoted-pair))

;; domain-literal = [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]
(define parse-domain-literal
  ($seq
   ($optional parse-cfws)
   ($char #\[)
   ($many
    ($seq ($optional parse-fws)
	  parse-dcontent))
   ($optional parse-fws)
   ($char #\])
   ($optional parse-cfws)))

;; obs-domain = atom *("." atom)
;; In practice, CFWS can appear around dots in obsolete syntax
;; Use $try to enable backtracking if CFWS consumed but no dot follows
(define parse-obs-domain
  ($seq
   parse-atom
   ($many
    ($try ($seq ($optional parse-cfws) ($char #\.) ($optional parse-cfws)
		parse-atom)))))

;; domain = dot-atom / domain-literal / obs-domain
;; Try domain-literal first (for [...]), then obs-domain (most general for regular domains)
(define parse-domain
  ($or ($try parse-domain-literal) parse-obs-domain))

;; addr-spec = local-part "@" domain
(define parse-addr-spec
  ($seq parse-local-part ($char #\@) parse-domain ($eos)))

(define (validate-domain-structure domain)
  (and (> (string-length domain) 0)
       (not (eqv? (string-ref domain 0) #\.))
       (not (eqv? (string-ref domain (- (string-length domain) 1)) #\.))
       (not (string-contains domain ".."))
       (let ((labels (string-split domain #\.)))
	 (let check-labels ((labels labels))
	   (or (null? labels)
	       (let ((label (car labels)))
		 (and (> (string-length label) 0)
		      (not (eqv? (string-ref label 0) #\-))
		      (not (eqv? (string-ref label
					     (- (string-length label) 1))
				 #\-))
		      (check-labels (cdr labels)))))))))

(define (valid-email-address? address)
  (and address
       (not (equal? address ""))
       (not (has-control-chars? address))
       (string-index address #\@)
       (guard (e (else #false))
	 (let ((result (peg-parse-string parse-addr-spec address)))
	   (and result
		(let* ((at-position (string-index address #\@))
		       (local-part (substring address 0 at-position))
		       (domain (substring address
					  (+ at-position 1)
					  (string-length address))))
		  (and (> (string-length local-part) 0)
		       (> (string-length domain) 0)
		       (validate-domain-structure domain))))))))