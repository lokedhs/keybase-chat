;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)
(require 'notifications)

(defvar keybase-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'keybase-insert-nl)
    (define-key map (kbd "RET") 'keybase-send-input-line)
    (define-key map (kbd "@") 'keybase-insert-user)
    (define-key map (kbd "C-c C-d") 'keybase-delete-message)
    map))

(define-derived-mode keybase-channel-mode nil "Keybase"
  "Mode for Keybase channel content"
  (use-local-map keybase-channel-mode-map)
  (setq-local keybase--output-marker (make-marker))
  (setq-local keybase--input-marker (make-marker))
  (set-marker keybase--output-marker (point-max))
  (insert "channel> ")
  (add-text-properties (point-at-bol) (point)
                       (list 'read-only t
                             'rear-nonsticky t
                             'front-sticky '(read-only)
                             'inhibit-line-move-field-capture t
                             'field 'output))
  (set-marker-insertion-type keybase--output-marker t)
  (set-marker keybase--input-marker (point-max)))

(defvar *keybase--proc-buf* nil)

(defun keybase--filter-command (proc output)
  (message "got output: %S" output))

(defun keybase--connect-to-server ()
  (let ((name "*keybase server*"))
    ;; Ensure that there is no buffer with this name already
   (when (get-buffer name)
     (error "keybase server buffer already exists"))
   (let ((buf (get-buffer-create name)))
     (let ((proc (make-process :name "keybase server"
                               :buffer buf
                               :command '("keybase" "chat" "api-listen")
                               :coding 'utf-8
                               :filter 'keybase--filter-command)))
       (with-current-buffer buf
         (setq *keybase--proc-buf* proc))
       buf))))

(defun keybase--find-active-process-buffer ()
  (when *keybase--proc-buf*
    (if (buffer-live-p *keybase--proc-buf*)
        (with-current-buffer *keybase--proc-buf*
          (if (process-live-p keybase--proc)
              *keybase--proc-buf*
            (progn
              (kill-buffer *keybase--proc-buf*)
              (setq *keybase--proc-buf* nil)
              nil)))
      (progn
        (setq *keybase--proc-buf* nil)
        nil))))

(defun keybase--find-process-buffer ()
  (let ((buf (keybase--find-active-process-buffer)))
    (or buf (keybase--connect-to-server))))

(provide 'keybase)
