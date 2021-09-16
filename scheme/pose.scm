;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define (pose-whitespace? char)
  (or (char=? #\x20 char) (char<=? #\x09 char #\x0D)))

(define (string-member? string char)
  (let loop ((i (- (string-length string) 1)))
    (and (>= i 0) (or (char=? char (string-ref string i))
                      (loop (- i 1))))))

(define (pose-token-common-char? char)
  (or (char<=? #\0 char #\9)
      (char<=? #\A char #\Z)
      (char<=? #\a char #\z)
      (string-member? "_$!?<=>+-*" char)))

(define (pose-token-first-char? char)
  (or (pose-token-common-char? char)
      (string-member? "/" char)))

(define (pose-token-next-char? char)
  (or (pose-token-first-char? char)
      (string-member? ".@~^%&" char)))

(define (make-float idigits fdigits fcount edigits)
  )

(define (parse-digits radix s i)
  #f)

(define (parse-sign-and-digits radix s i)
  (cond ((looking-at? #\+ s i) (+ (parse-digits radix s (+ i 1))))
        ((looking-at? #\- s i) (- (parse-digits radix s (+ i 1))))
        (else (parse-digits radix s i))))

(define (parse-decimal-or-symbol s)
  (let*-values
      (((i ipart) (parse-sign-and-digits 10 s 0))
       ((i fpart) (and ipart (< i n) (char=?
                       (if . (parse-digits 10 s i))))
       ((i epart) (and ipart (if e (parse-sign-and-digits 10 s 0))))
       ((done?)  (= i len)))
    (cond ((not ipart) (string->symbol s))
          ((not done?) (error "Bad decimal number" s))
          ((or fpart epart) (make-float ipart fpart epart))
          (else ipart))))

(define (skip-rest-of-line)
  (let ((char (read-char)))
    (unless (or (eof-object? char) (char=? #\newline char))
      (skip-rest-of-line))))

(define (skip-whitespace-and-comments)
  (let ((char (peek-char)))
    (cond ((eof-object? char) #f)
          ((char=? #\; char)
           (skip-rest-of-line)
           (skip-whitespace-and-comments))
          ((pose-whitespace? char)
           (read-char)
           (skip-whitespace-and-comments)))))

(define (read-token-as-string)
  (let ((char (read-char)))
    (unless (pose-token-first-char? char)
      (error "Not a token first char" char))
    (let loop ((chars (list char)))
      (let ((char (peek-char)))
        (if (or (eof-object? char) (not (pose-token-next-char? char)))
            (list->string (reverse chars))
            (loop (cons (read-char) chars)))))))

(define (read-sharpsign)
  (let* ((radix (let ((char (read-char)))
                  (case char
                    ((#\b) 2)
                    ((#\o) 8)
                    ((#\x) 16)
                    (else (error "Unknown #" char)))))
         (token (read-token-as-string))
         (ipart (parse-sign-and-digits radix s 0)))
    (if (and ipart done?) ipart
        (error "Cannot parse integer from token" token radix))))

(define (read-delimited-list end-char)
  (let loop ((forms '()))
    (skip-whitespace-and-comments)
    (cond ((eqv? end-char (peek-char))
           (read-char)
           (reverse forms))
          (else
           (let ((form (pose-read)))
             (if (eof-object? form) (error "Unterminated list")
                 (loop (cons form forms))))))))

(define (read-delimited-string end-char)
  (let loop ((chars '()))
    (let ((char (read-char)))
      (cond ((eof-object? char)
             (error "Unterminated string" end-char))
            ((char=? end-char char)
             (list->string (reverse chars)))
            ((char=? #\\ char)
             (let ((char (read-char)))
               (if (eof-object? char)
                   (error "Unterminated string escape")
                   (loop
                    (cons (case char
                            ((#\n) #\newline)
                            ((#\t) #\tab)
                            ((#\\ #\| #\") char)
                            (else (error "Unknown string escape" char)))
                          chars)))))
            (else
             (loop (cons char chars)))))))

(define (pose-read)
  (skip-whitespace-and-comments)
  (let ((char (peek-char)))
    (cond ((eof-object? char)
           (eof-object))
          ((pose-token-first-char? char)
           (parse-number-or-symbol (read-token-as-string)))
          (else
           (let ((char (read-char)))
             (case char
               ((#\#) (read-sharpsign))
               ((#\|) (string->symbol (read-delimited-string #\|)))
               ((#\") (read-delimited-string #\"))
               ((#\() (read-delimited-list #\)))
               ((#\)) (error "Stray closing parenthesis"))
               (else  (error "Unknown character at top level" char))))))))

(define (pose-read-all)
  (let loop ((forms '()))
    (let ((form (pose-read)))
      (if (eof-object? form) (reverse forms)
          (loop (cons form forms))))))
