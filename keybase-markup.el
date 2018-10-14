;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'cl)

(defvar keybase--markup-allow-nl nil)

(defun keybase--trim-blanks-and-newlines (s)
  (string-trim s))

(cl-defmacro keybase--when-trimmed-not-empty ((sym string) &body body)
  (declare (indent 1))
  (let ((trimmed (gensym)))
    `(let ((,trimmed (keybase--trim-blanks-and-newlines ,string)))
       (when (plusp (length ,trimmed))
         (let ((,sym ,trimmed))
           ,@body)))))

(defun keybase--find-reg-starts-ends ()
  (loop for i from 1
        for start = (match-beginning i)
        while start
        collect start into sindex
        collect (match-end i) into eindex
        finally (return (list sindex eindex))))

(defun keybase--process-regex-parts (regexp string match-fn no-match-fn)
  (loop with length = (length string)
        with start = 0
        while (< start length)
        do (let ((match-start (string-match regexp string start)))
             (if match-start
                 ;; Match found, possibly call the no-match function for the segment before the match
                 (let ((match-end (match-end 0)))
                   (destructuring-bind (reg-starts reg-ends)
                       (keybase--find-reg-starts-ends)
                     (when (> match-start start)
                       (funcall no-match-fn start match-start))
                     (funcall match-fn reg-starts reg-ends)
                     (setq start match-end)))
               ;; ELSE: No match, call the no-match function for the last segment
               (progn
                 (funcall no-match-fn start length)
                 (setq start length))))))

(defun keybase--markup-from-regexp (regexp string callback &optional plain-string-markup-fn)
  (cl-flet ((markup-string (s)
                           (if plain-string-markup-fn
                               (funcall plain-string-markup-fn s)
                             (list s))))
    (loop with length = (length string)
          with start = 0
          while (< start length)
          append (let ((match-start (string-match regexp string start)))
                   (if match-start
                       ;; Some highlighted text was found
                       (let ((match-end (match-end 0)))
                         (destructuring-bind (reg-starts reg-ends)
                             (keybase--find-reg-starts-ends)
                           (let* ((highlight (funcall callback reg-starts reg-ends))
                                  (old-start start))
                             (setq start match-end)
                             (if (> match-start old-start)
                                 ;; There is some unmatched text before the match
                                 (append (markup-string (subseq string old-start match-start))
                                         (list highlight))
                               ;; ELSE: The match is at the beginning of the string
                               (list highlight)))))
                     ;; ELSE: No match, copy the last part of the text and finish the loop
                     (let ((old-start start))
                       (setq start length)
                       (markup-string (subseq string old-start))))))))

(defun keybase--strip-indentation-char (string)
  (with-output-to-string (out)
    (process-regex-parts "(?ms)^> *([^\\n]*)" string
                         (lambda (reg-starts reg-ends)
                           (princ (subseq string (aref reg-starts 0) (aref reg-ends 0)) out))
                         (lambda (start end)
                           (princ (subseq string start end) out)))))

(defun keybase--markup-indent (string)
  (markup-from-regexp "\\n?\\(\\(?:^>[^\\n]*\\)\\(?:\\n>[^\\n]*\\)*\\)\\n?" string
                      (lambda (reg-starts reg-ends)
                        (let ((text (keybase--strip-indentation-char (subseq string (aref reg-starts 0) (aref reg-ends 0)))))
                          (list (cons :quote (keybase--markup-paragraphs-inner text)))))))

(defun keybase--markup-codeblocks (string)
  (loop with result = nil
        with state = :normal
        with current-language = nil
        with length = (length string)
        with pos = 0
        while (< pos length)
        if (eq state :normal)
        do (let ((start (string-match "^```[ ]*\\([^\\n\\r ]*\\)[ ]*$" string pos)))
             (if start
                 (let ((end (match-end 0))
                       (s (match-beginning 1))
                       (e (match-end 1)))
                   (when (> start pos)
                     (keybase--when-trimmed-not-empty (trimmed (subseq string pos start))
                       (push trimmed result)))
                   (setq state :code)
                   (setq current-language (if (> e s) (subseq string s e) nil))
                   (setq pos end))
               (progn
                 (keybase--when-trimmed-not-empty (trimmed (subseq string pos length))
                   (push trimmed result))
                 (setq pos length))))
        else if (eq state :code)
        do (let ((start (string-match "^```[ ]*$" string pos)))
             (if start
                 (let ((end (match-end 0)))
                   (when (> start pos)
                     (keybase--when-trimmed-not-empty (trimmed (subseq string pos start))
                       (push (list (list :code-block current-language trimmed)) result)))
                   (setq state :normal)
                   (setq current-language nil)
                   (setq pos end))
               (progn
                 (keybase--when-trimmed-not-empty (trimmed (subseq string pos length))
                   (push trimmed result))
                 (setq pos length))))
        finally (return (reverse result))))

(defun keybase--select-blocks (string fn next-fn)
  (loop for v in (funcall fn string)
        append (if (stringp v)
                   (funcall next-fn v)
                 v)))

;; TODO: This is not converted yet
(defun keybase--markup-string (string)
  (if keybase--markup-allow-nl
      (loop for line in (split-sequence:split-sequence #\Newline string)
            for first = t then nil
            unless first
            append '((:newline))
            append (markup-custom-1 line))
    (markup-custom-1 string)))

(defun keybase--markup-paragraphs-inner (string)
  (loop with pos = 0
        with length = (length string)
        with result = nil
        while (< pos length)
        do (let ((match-start (string-match "\n\n" string start)))
             (if match-start
                 (let ((match-end (match-end 0)))
                   (when (< pos match-start)
                     (push (cons :paragraph (keybase--markup-string (subseq string pos match-start)))))
                   (setq pos match-end))
               ;; ELSE: No more matches, collect the last paragraph
               (push (cons :paragraph (keybase--markup-string (subseq string pos length))))))))

(defun keybase--markup-paragraphs (string &key allow-nl)
  (let ((keybase--markup-allow-nl allow-nl))
    (keybase--select-blocks string #'keybase--markup-codeblocks
                            (lambda (s)
                              (keybase--select-blocks s #'keybase--markup-indent #'keybase--markup-paragraphs-inner)))))
