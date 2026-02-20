;;;; Strict binary email address validator for user input
;;;; for Chibi Scheme
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

;; FWS (Folding White Space) - simplified, no CRLF allowed in strict mode
(define parse-fws
  (parse-repeat+ (parse-char char-wsp?)))

;; quoted-pair = "\" (VCHAR / WSP)
(define parse-quoted-pair
  (parse-seq (parse-char #\\)
             (parse-or (parse-char char-vchar?)
                       (parse-char char-wsp?))))

;; comment (recursive) - simplified
(define parse-comment
  (letrec ((comment-parser
            (lambda (source index sk fk)
              ((parse-seq
                (parse-char #\()
                (parse-repeat
                 (parse-or
                  (parse-char char-ctext?)
                  (parse-char char-wsp?)
                  parse-quoted-pair
                  comment-parser))
                (parse-char #\)))
               source index sk fk))))
    comment-parser))

;; CFWS = (1*([FWS] comment) [FWS]) / FWS
(define parse-cfws
  (parse-or
   (parse-seq
    (parse-repeat+
     (parse-seq (parse-optional parse-fws)
                parse-comment))
    (parse-optional parse-fws))
   parse-fws))

;; atom = [CFWS] 1*atext [CFWS]
(define parse-atom
  (parse-seq
   (parse-optional parse-cfws)
   (parse-repeat+ (parse-char char-atext?))
   (parse-optional parse-cfws)))

;; dot-atom-text = 1*atext *("." 1*atext)
(define parse-dot-atom-text
  (parse-seq
   (parse-repeat+ (parse-char char-atext?))
   (parse-repeat
    (parse-seq (parse-char #\.)
               (parse-repeat+ (parse-char char-atext?))))))

;; dot-atom = [CFWS] dot-atom-text [CFWS]
(define parse-dot-atom
  (parse-seq
   (parse-optional parse-cfws)
   parse-dot-atom-text
   (parse-optional parse-cfws)))

;; qcontent = qtext / quoted-pair
(define parse-qcontent
  (parse-or
   (parse-char char-qtext?)
   parse-quoted-pair))

;; quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
(define parse-quoted-string
  (parse-seq
   (parse-optional parse-cfws)
   (parse-char #\")
   (parse-repeat
    (parse-seq (parse-optional parse-fws)
               parse-qcontent))
   (parse-optional parse-fws)
   (parse-char #\")
   (parse-optional parse-cfws)))

;; word = atom / quoted-string
(define parse-word
  (parse-or parse-atom parse-quoted-string))

;; obs-local-part = word *("." word)
(define parse-obs-local-part
  (parse-seq
   parse-word
   (parse-repeat
    (parse-seq (parse-char #\.)
               parse-word))))

;; local-part = dot-atom / quoted-string / obs-local-part
(define parse-local-part
  (parse-or parse-dot-atom parse-quoted-string parse-obs-local-part))

;; dtext-no-obs = %d33-90 / %d94-126
(define parse-dtext
  (parse-char char-dtext?))

;; dcontent = dtext / quoted-pair
(define parse-dcontent
  (parse-or parse-dtext parse-quoted-pair))

;; domain-literal = [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]
(define parse-domain-literal
  (parse-seq
   (parse-optional parse-cfws)
   (parse-char #\[)
   (parse-repeat
    (parse-seq (parse-optional parse-fws)
               parse-dcontent))
   (parse-optional parse-fws)
   (parse-char #\])
   (parse-optional parse-cfws)))

;; obs-domain = atom *("." atom)
(define parse-obs-domain
  (parse-seq
   parse-atom
   (parse-repeat
    (parse-seq (parse-char #\.)
               parse-atom))))

;; domain = dot-atom / domain-literal / obs-domain
(define parse-domain
  (parse-or parse-dot-atom parse-domain-literal parse-obs-domain))

;; addr-spec = local-part "@" domain
(define parse-addr-spec
  (parse-seq parse-local-part (parse-char #\@) parse-domain parse-end))

(define (string-find-char string character)
  (let ((cursor (string-find string character)))
    (and (not (string-cursor=? cursor (string-cursor-end string)))
	 (string-length (substring string 0 cursor)))))

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
       (string-find-char address #\@)
       (let ((result (parse parse-addr-spec address)))
	 (and result
	      (let* ((at-position (string-find-char address #\@))
		     (local-part (substring address 0 at-position))
		     (domain (substring address
					(+ at-position 1)
					(string-length address))))
		(and (> (string-length local-part) 0)
		     (> (string-length domain) 0)
		     (validate-domain-structure domain)))))))