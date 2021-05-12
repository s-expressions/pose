;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: ISC

(defconst pose--eof (gensym "pose--eof-"))

(defun pose--whitespace-char-p (char)
  (or (eql ?\s char)
      (eql ?\t char)
      (eql ?\n char)))

(defun pose--token-first-char-p (char)
  (or (<= ?A char ?Z)
      (<= ?a char ?z)
      (<= ?0 char ?9)
      (not (not (string-match-p "[_$!?<=>+*/-]" (string char))))))

(defun pose--token-next-char-p (char)
  (or (pose--token-first-char-p char)
      (not (not (string-match-p "[.@~^%&]" (string char))))))

(defun pose--string-to-symbol (string)
  (make-symbol string))

(defun pose--parse-integer (string radix)
  nil)

(defun pose--parse-number-or-symbol (string)
  (pose--string-to-symbol string))

(defun pose--peek-char ()
  (if (eobp) nil (char-after)))

(defun pose--read-char ()
  (if (eobp) nil (prog1 (char-after) (goto-char (1+ (point))))))

(defun pose--skip-rest-of-line ()
  (while (let ((char (pose--read-char)))
           (and char (not (eql ?\n char))))))

(defun pose--skip-whitespace-and-comments ()
  (while (let ((char (pose--peek-char)))
           (cond ((not char)
                  nil)
                 ((eql ?\; char)
                  (pose--skip-rest-of-line)
                  t)
                 ((pose--whitespace-char-p char)
                  (pose--read-char)
                  t)
                 (t nil)))))

(defun pose--read-token-as-string ()
  (let ((char (pose--read-char)))
    (unless (pose--token-first-char-p char)
      (error "Not a token first char: %s" char))
    (let ((chars (string char)))
      (while (let ((char (pose--peek-char)))
               (cond ((and char (pose--token-next-char-p char))
                      (setq chars (concat chars (string (pose--read-char)))))
                     (t nil))))
      chars)))

(defun pose--read-sharpsign ()
  (error "TODO"))

(defun pose--read-delimited-list (end-char)
  (let ((forms '()))
    (while (progn (pose--skip-whitespace-and-comments)
                  (not (when (eql end-char (pose--peek-char))
                         (pose--read-char)
                         t)))
      (let ((form (pose--read-or-eof)))
        (if (eq pose--eof form)
            (error "Unterminated list")
          (push form forms))))
    (reverse forms)))

(defun pose--read-string-escape ()
  (let ((char (pose--read-char)))
    (if (eql pose--eof char)
        (error "Unterminated string escape")
      (case char
        ((?\n) "\n")
        ((?\t) "\t")
        ((?\\ ?\| ?\") (string char))
        (t (error "Unknown string escape: %s" (string char)))))))

(defun pose--read-delimited-string (end-char)
  (let ((chars "") (more t))
    (while more
      (let ((char (pose--read-char)))
        (cond ((eql pose--eof char)
               (error "Unterminated string, expecting %s" end-char))
              ((eql end-char char)
               (setq more nil))
              ((eql ?\\ char)
               (setq chars (concat chars (pose--read-string-escape))))
              (t
               (setq chars (concat chars (string char)))))))
    chars))

(defun pose--read-or-eof ()
  (pose--skip-whitespace-and-comments)
  (let ((char (pose--peek-char)))
    (cond ((not char)
           pose--eof)
          ((pose--token-first-char-p char)
           (pose--parse-number-or-symbol (pose--read-token-as-string)))
          (t
           (let ((char (pose--read-char)))
             (case char
               ((?\#) (pose--read-sharpsign))
               ((?\|) (pose--string-to-symbol
                       (pose--read-delimited-string ?\|)))
               ((?\") (pose--read-delimited-string ?\"))
               ((?\() (pose--read-delimited-list ?\)))
               ((?\)) (error "Stray closing parenthesis"))
               (else  (error "Unknown character at top level: %S" char))))))))

(defun pose-read ()
  (let ((form (pose--read-or-eof)))
    (if (eql pose--eof form) (signal 'end-of-file '()) form)))

(defun pose-read-all ()
  (let (forms form)
    (while (not (eql pose--eof (setq form (pose--read-or-eof))))
      (push form forms))
    (reverse forms)))

(defun pose--write-with-vertical-bars-p (string)
  nil)

(provide 'pose)
