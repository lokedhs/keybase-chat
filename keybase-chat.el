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

(defun keybase--handle-incoming-chat-message (json)
  (message "Got json: %S" json))

(defvar *keybase--proc-buf* nil)

(defun keybase--json-find (obj path)
  (let ((curr obj))
    (loop for path-entry in path
          for node = (assoc path-entry curr)
          unless node
          do (error "Node not found in json: %S" path-entry)
          do (setq curr (cdr node)))
    curr))

(defun keybase--request-api (arg)
  (let ((output-buf (generate-new-buffer " *keybase api*")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert (json-serialize arg))
            (call-process-region (point-min) (point-max) "keybase" nil output-buf nil "chat" "api"))
          (with-current-buffer output-buf
            (goto-char (point-min))
            (json-parse-buffer :object-type 'alist)))
      (kill-buffer output-buf))))

(defun keybase--list-channels ()
  (let ((result (keybase--request-api '((method . "list")))))
    (loop for conversation across (keybase--json-find result '(result conversations))
          collect (list (keybase--json-find conversation '(id))
                        (keybase--json-find conversation '(channel name))
                        (keybase--json-find conversation '(channel topic_name))))))

(cl-defun keybase--filter-command (proc output)
  ;; Hack to skip the initial status message. This message is sent on
  ;; stderr so it should never be seen, but this function is still
  ;; called when it's added.
  (when (string-match "^Listening for chat notifications" output)
    (return-from keybase--filter-command nil))
  ;;
  (with-current-buffer (process-buffer proc)
    (save-excursion
      ;; Add the output to the buffer
      (goto-char (point-max))
      (insert output)
      ;; Parse any completed messages
      (goto-char (point-min))
      (loop with pos = (point)
            for nl = (search-forward-regexp "\n" nil t)
            while nl
            do (let ((content (buffer-substring pos nl)))
                 (keybase--handle-incoming-chat-message (json-read-from-string content))))
      (delete-region (point-min) (point)))))

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
         (setq-local *keybase--server-process* proc)
         (setq-local *keybase--channels* nil))
       (setq *keybase--proc-buf* buf)
       buf))))

(defun keybase--find-active-process-buffer ()
  (when *keybase--proc-buf*
    (if (buffer-live-p *keybase--proc-buf*)
        (with-current-buffer *keybase--proc-buf*
          (if (process-live-p *keybase--server-process*)
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

(defun keybase--disconnect-from-server ()
  (let ((buf (keybase--find-active-process-buffer)))
    (when buf
      (kill-buffer buf)
      (setq *keybase--proc-buf* nil))))

(defun keybase--choose-channel-info ()
  (let ((channels (keybase--list-channels)))
    (destructuring-bind (names-list names-ref)
        (loop for channel in channels
              for name = (format "%s/%s" (second channel) (third channel))
              collect name into names-list
              collect (list name channel) into id-list
              finally (return (list names-list id-list)))
      (let ((result (completing-read "Channel: " names-list nil t nil nil nil nil)))
        (unless result
          (error "No channel was selected"))
        (let ((found (cl-find result names-ref :key #'first :test #'equal)))
          (unless found
            (error "Selected channel did not match one of available names"))
          (second found))))))

(defun keybase-join-channel (channel-info)
  (interactive (list (keybase--choose-channel-info))))

(provide 'keybase)
