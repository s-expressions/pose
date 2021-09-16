;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: ISC

(defpackage #:pose
  (:use #:cl)
  (:shadow #:read)
  (:export #:read #:read-all #:*make-symbol*))

(in-package #:pose)

(defvar +pose-eof+ (gensym "POSE-EOF-"))

(defvar *make-symbol* #'intern)

(defun pose-make-symbol (string)
  (funcall *make-symbol* string))

(defun pose-whitespace-char-p (char)
  (case char ((#\space #\tab #\newline #\return) t)))

(defun pose-token-common-char-p (char)
  (or (char<= #\0 char #\9)
      (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (not (null (position char "_$!?<=>+-*")))))

(defun pose-token-first-char-p (char)
  (or (pose-token-common-char-p char)
      (not (null (position char "/")))))

(defun pose-token-next-char-p (char)
  (or (pose-token-first-char-p char)
      (not (null (position char ".@~^%&")))))

(defun skip-char? (match)
  (let ((char (peek-char nil nil nil)))
    (and char
         (if (functionp match) (funcall match char) (char= match char))
         (read-char))))

(defun skip-char* (match)
  (and (skip-char? match)
       (loop (unless (skip-char? match) (return t)))))

(defun pose-valid-number-p (string)
  (with-input-from-string (*standard-input* string)
    (labels ((zero-to-nine-p (char)
               (char<= #\0 char #\9))
             (one-to-nine-p (char)
               (char<= #\1 char #\9))
             (dot-p (char)
               (char= #\. char))
             (sign-p (char)
               (or (char= #\+ char) (char= #\- char)))
             (exponent-marker-p (char)
               (or (char= #\e char) (char= #\E char)))
             (read-fraction ()
               (and (skip-char? #\.)
                    (or (skip-char* #'zero-to-nine-p)
                        (return-from pose-valid-number-p nil))))
             (read-exponent ()
               (and (skip-char? #'exponent-marker-p)
                    (progn (skip-char? #'sign-p)
                           (or (skip-char* #'zero-to-nine-p)
                               (return-from pose-valid-number-p nil)))))
             (read-fraction-exponent ()
               (read-fraction)
               (read-exponent)
               (null (peek-char nil nil nil)))
             (read-magnitude-fraction-exponent ()
               (if (skip-char? #\0)
                   (read-fraction-exponent)
                   (and (skip-char? #'one-to-nine-p)
                        (progn (skip-char* #'zero-to-nine-p)
                               (read-fraction-exponent))))))
      (skip-char? #\-)
      (read-magnitude-fraction-exponent))))

(defun parse-number-or-symbol (string)
  (or (and (pose-valid-number-p string)
           (read-from-string string))
      (pose-make-symbol string)))

(defun skip-rest-of-line (stream)
  (loop (let ((char (read-char stream nil)))
          (when (or (null char) (char= #\newline char))
            (return)))))

(defun skip-whitespace-and-comments (stream)
  (loop (let ((char (peek-char nil stream nil)))
          (cond ((null char)
                 (return))
                ((char= #\; char)
                 (skip-rest-of-line stream))
                ((pose-whitespace-char-p char)
                 (read-char stream nil))
                (t
                 (return))))))

(defun read-token-as-string (stream)
  (let ((char (read-char stream nil)))
    (unless (pose-token-first-char-p char)
      (error "Not a token first char: ~S" char))
    (with-output-to-string (out)
      (write-char char out)
      (loop (let ((char (peek-char nil stream nil)))
              (unless (and char (pose-token-next-char-p char))
                (return))
              (read-char stream nil)
              (write-char char out))))))

(declaim (ftype (function (stream) t)
                read-internal))

(defun pose-read-delimited-list (end-char stream)
  (let ((forms '()))
    (loop (progn (skip-whitespace-and-comments stream)
                 (cond ((eql end-char (peek-char nil stream nil))
                        (read-char stream nil)
                        (return (reverse forms)))
                       (t
                        (let ((form (read-internal stream)))
                          (if (eq +pose-eof+ form) (error "Unterminated list")
                              (push form forms)))))))))

(defun read-string-escape (end-char stream)
  (let ((char (read-char stream nil)))
    (if (eql end-char char) char
        (case char
          ((#\n) #\newline)
          ((#\t) #\tab)
          ((#\\) char)
          (t (error "Unknown string escape: ~S" char))))))

(defun read-delimited-string (end-char stream)
  (with-output-to-string (out)
    (loop (let ((char (read-char stream nil)))
            (cond ((null char)
                   (error "Unterminated string: ~S" end-char))
                  ((char= end-char char)
                   (return))
                  ((char= #\\ char)
                   (write-char (read-string-escape end-char stream) out))
                  (t
                   (write-char char out)))))))

(defun read-internal (stream)
  (skip-whitespace-and-comments stream)
  (let ((char (peek-char nil stream nil)))
    (cond ((null char)
           +pose-eof+)
          ((pose-token-first-char-p char)
           (parse-number-or-symbol (read-token-as-string stream)))
          (t
           (let ((char (read-char stream nil)))
             (case char
               ((#\") (read-delimited-string #\" stream))
               ((#\() (pose-read-delimited-list #\) stream))
               ((#\)) (error "Stray closing parenthesis"))
               (t     (error "Unknown character at top level: ~S" char))))))))

(defun read (&optional stream eof-error-p eof-value)
  (let ((stream (or stream *standard-input*)))
    (let ((form (read-internal stream)))
      (cond ((not (eq +pose-eof+ form)) form)
            (eof-error-p (error 'end-of-file))
            (t eof-value)))))

(defun read-all (&optional stream)
  (let ((stream (or stream *standard-input*)))
    (let ((forms '()))
      (loop (let ((form (read-internal stream)))
              (if (eq +pose-eof+ form) (return (reverse forms))
                  (push form forms)))))))
