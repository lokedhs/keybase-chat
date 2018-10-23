;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'cl)

(defvar keybase--markup-allow-nl nil)
(defvar keybase--custom-parser-1 nil)
(defvar keybase--custom-parser-2 nil)
(defvar keybase--custom-parser-3 nil)

(defun keybase--trim-blanks-and-newlines (s)
  (string-trim s))

(cl-defmacro keybase--when-trimmed-not-empty ((sym string) &body body)
  (declare (indent 1))
  (let ((trimmed (gensym)))
    `(let ((,trimmed (keybase--trim-blanks-and-newlines ,string)))
       (when (plusp (length ,trimmed))
         (let ((,sym ,trimmed))
           ,@body)))))

(cl-defun keybase--split-with-regexp (regexp string &key empty)
  (loop with pos = 0
        with length = (length string)
        with result = nil
        while (< pos length)
        do (let ((match-start (string-match regexp string pos)))
             (if match-start
                 (let ((match-end (match-end 0)))
                   (when (or (< pos match-start) empty)
                     (push (subseq string pos match-start) result))
                   (setq pos match-end))
               ;; ELSE: No more matches, collect the last paragraph
               (progn
                 (push (subseq string pos length) result)
                 (setq pos length))))
        finally (return (reverse result))))

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
  (keybase--markup-from-regexp "\\n?\\(\\(?:^>[^\\n]*\\)\\(?:\\n>[^\\n]*\\)*\\)\\n?" string
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

(defun keybase--markup-highlight (string)
  (let ((pos 0)
        (length (length string))
        (result nil))
    (cl-labels ((collect-part (v) (when (< pos v) (setq result (append result (keybase--markup-custom-3 (subseq string pos v)))))))
      (loop while (< pos length)
            do (let ((match-start (string-match "\\(?:^\\|\\W\\)\\([*_]\\)\\(.+?\\)\\(\\1\\)\\(?:$\\|\\W\\)" string pos)))
                 (if match-start
                     (let ((scode-s (match-beginning 1))
                           (ecode-e (match-end 3))
                           (s (match-beginning 2))
                           (e (match-end 2)))
                       (collect-part scode-s)
                       (let ((code (aref string scode-s)))
                         (setq result (append result (list (cons (cond ((eql code ?*)
                                                                        :bold)
                                                                       ((eql code ?_)
                                                                        :italics)
                                                                       (t
                                                                        (error "Unexpected code")))
                                                                 (keybase--markup-custom-3 (subseq string s e)))))))
                       (setq pos ecode-e))
                   ;; ELSE: No more matches
                   (progn
                     (collect-part length)
                     (setq pos length)))))
      result)))

(defun keybase--markup-custom-1 (string)
  (if keybase--custom-parser-1
      (funcall keybase--custom-parser-1 string #'keybase--markup-maths)
      (keybase--markup-maths string)))

(defun keybase--markup-custom-2 (string)
  (if keybase--custom-parser-2
      (funcall keybase--custom-parser-2 string #'keybase--markup-highlight)
    (keybase--markup-highlight string)))

(defun keybase--markup-custom-3 (string)
  (if keybase--custom-parser-3
      (funcall keybase--custom-parser-3 string (lambda (v) (list v)))
    (list string)))

(defun keybase--markup-url (string)
  (keybase--markup-custom-2 string))

(defun keybase--markup-maths (string)
  ;; Maths needs to be extracted before anything else, since it can
  ;; contain a mix of pretty much every other character, and we don't
  ;; want that to mess up any other highlighting.
  (let ((pos 0)
        (length (length string))
        (result nil))
    (cl-labels ((collect-part (v)
                              (when (< pos v)
                                (setq result (append result (keybase--markup-url (subseq string pos v)))))))
      (loop while (< pos length)
            do (let ((match-start (string-match "\\(?:^\\|\\W\\)\\(`[^`]+`\\)\\(?:$\\|\\W\\)" string pos)))
                 (if match-start
                     ;; We don't do maths markup right now, but once we do, it should be done here
                     (let ((s (match-beginning 1))
                           (e (match-end 1)))
                       (collect-part s)
                       (setq result (append result (list (cons :code (subseq string (1+ s) (1- e))))))
                       (setq pos e))
                   ;; ELSE: No more matches
                   (progn
                     (collect-part length)
                     (setq pos length)))))
      result)))

(defun keybase--markup-string (string)
  (if keybase--markup-allow-nl
      (loop for line in (keybase--split-with-regexp "\n" string :empty t)
            for first = t then nil
            unless first
            append '((:newline))
            unless (equal line "")
            append (keybase--markup-custom-1 line))
    (keybase--markup-custom-1 string)))

(defun keybase--markup-paragraphs-inner (string)
  (loop
   for v in (keybase--split-with-regexp "\n\\{2,\\}" string)
   when (plusp (length v))
   collect (cons :paragraph (keybase--markup-string v))))

(cl-defun keybase--markup-paragraphs (string &key allow-nl)
  (let ((keybase--markup-allow-nl allow-nl))
    (keybase--select-blocks string #'keybase--markup-codeblocks
                            (lambda (s)
                              (keybase--select-blocks s #'keybase--markup-indent #'keybase--markup-paragraphs-inner)))))

(provide 'keybase-markup)
