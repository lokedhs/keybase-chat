;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)
(require 'notifications)

(defgroup keybase nil
  "Keybase chat implementation"
  :prefix 'keybase
  :group 'applications)

(defface keybase-default
  ()
  "Default face for chat buffers."
  :group 'keybase)

(defface keybase-message-from
  '((((class color))
     :foreground "#00b000"
     :inherit keybase-default)
    (t
     :inherit keybase-default))
  "Face used to display the 'from' part of a message."
  :group 'keybase)

(cl-defun keybase--json-find (obj path &key (error-if-missing t))
  (let ((curr obj))
    (loop for path-entry in path
          for node = (assoc path-entry curr)
          unless node
          do (if error-if-missing
                 (error "Node not found in json: %S" path-entry)
               (return nil))
          do (setq curr (cdr node))
          finally (return curr))))

(defvar *keybase--proc-buf* nil)
(defvar *keybase--active-buffers* nil
  "List of active channels.
Each entry is of the form (CHANNEL-INFO BUFFER)")

(defvar keybase-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'keybase-insert-nl)
    (define-key map (kbd "RET") 'keybase-send-input-line)
    ;;(define-key map (kbd "@") 'keybase-insert-user)
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

(defun keybase--read-input-line (start end)
  (let ((uid-refs (loop for overlay in (overlays-in start end)
                        for uid = (overlay-get overlay 'keybase-user-ref)
                        when uid
                        collect (list (overlay-start overlay) (overlay-end overlay) uid overlay))))
    (with-output-to-string
      (loop with p = start
            for uid-ref in (sort uid-refs (lambda (a b) (< (first a) (first b))))
            if (< p (first uid-ref))
            do (princ (buffer-substring p (first uid-ref)))
            do (progn
                 (error "uid-refs not implemented")
                 (princ (format "\U000f0001user:%s:%s\U000f0001"
                                (third uid-ref) (buffer-substring (first uid-ref) (second uid-ref))))
                 (setq p (second uid-ref))
                 (delete-overlay (fourth uid-ref)))
            finally (when (< p end)
                      (princ (buffer-substring p end)))))))

(defun keybase-insert-nl ()
  "Insert a newline into the message."
  (interactive)
  (insert "\n"))

(defun keybase-insert-user ()
  "Select a username to be inserted into the new message."
  (interactive)
  (error "can't insert user"))

(defun keybase-delete-message ()
  (interactive)
  (let ((msgid (keybase--find-message-at-point (point))))
    (if msgid
        (when (yes-or-no-p "Really delete message? ")
          (keybase--request-api `((method . "delete")
                                  (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                         (message_id . ,msgid))))))))
      ;; ELSE: No message at point
      (message "No message at point"))))

(defun keybase--buffer-closed ()
  (setq *keybase--active-buffers* (cl-remove (current-buffer) *keybase--active-buffers* :key #'cdr :test #'eq)))

(defun keybase--create-buffer (channel-info)
  ;; First ensure that the listener is running
  (keybase--find-process-buffer)
  ;; Create the buffer
  (let ((buffer (generate-new-buffer (format "*keybase %s - %s*" (first channel-info) (second channel-info)))))
    (with-current-buffer buffer
      (keybase-channel-mode)
      (setq-local keybase--channel-info channel-info)
      (add-hook 'kill-buffer-hook 'keybase--buffer-closed nil t)
      (push (cons channel-info buffer) *keybase--active-buffers*)
      (keybase--load-initial-messages)
      buffer)))

(cl-defun keybase--find-channel-buffer (channel-info &key (if-missing :error))
  (unless (member if-missing '(:error :create :ignore))
    (error "Illegal argument to if-missing: %S" if-missing))
  (let ((e (find channel-info *keybase--active-buffers* :key #'car :test #'equal)))
    (cond (e
           (cdr e))
          ((eq if-missing :create)
           (keybase--create-buffer channel-info))
          ((eq if-missing :error)
           (error "No buffer for channel %S" channel-info)))))

(defun keybase--load-initial-messages ()
  (let* ((messages-json (keybase--request-api `((method . "read")
                                                (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                                       (pagination . ((num . 10))))))))))
         (messages (keybase--json-find messages-json '(result messages))))
    (loop for msg-entry across messages
          for msg = (keybase--json-find msg-entry '(msg))
          for id = (keybase--json-find msg '(id))
          for sender = (keybase--json-find msg '(sender username))
          for timestamp = (keybase--json-find msg '(sent_at))
          for content = (keybase--json-find msg '(content))
          for type = (keybase--json-find content '(type))
          when (equal type "text")
          do (keybase--insert-message id timestamp sender (keybase--json-find content '(text body))))))

(defun keybase--format-date (timestamp)
  (let ((time (seconds-to-time timestamp)))
    (format-time-string "%Y-%m-%d %H:%M:%S" time)))

(defun keybase--insert-message-content (pos id timestamp sender message)
  (goto-char pos)
  (let ((inhibit-read-only t))
    (let ((start (point)))
      (insert (propertize (format "[%s] %s\n" sender (keybase--format-date timestamp))
                          'face 'keybase-message-from))
      (when (> (length message) 0)
        (insert (concat message "\n\n")))
      (add-text-properties start (point)
                           (list 'read-only t
                                 'keybase-message-id id
                                 'keybase-timestamp timestamp
                                 'keybase-sender sender
                                 'front-sticky '(read-only))))))

(defun keybase--insert-message (id timestamp sender message)
  (save-excursion
    (goto-char keybase--output-marker)
    (let ((new-pos (loop with prev-pos = (point)
                         for pos = (previous-single-char-property-change prev-pos 'keybase-timestamp)
                         until (let ((prop (get-char-property pos 'keybase-timestamp)))
                                 (and prop (< prop timestamp)))
                         do (setq prev-pos pos)
                         until (= pos (point-min))
                         finally (return prev-pos))))
      (keybase--insert-message-content new-pos id timestamp sender message))))

(defun keybase--find-message-in-log (id)
  (loop with curr = (point-min)
        for pos = (next-single-property-change curr 'keybase-message-id)
        while pos
        for value = (get-char-property pos 'keybase-message-id)
        when (equal value id)
        return (list pos (next-single-property-change pos 'keybase-message-id))
        do (setq curr pos)
        finally (return nil)))

(defun keybase--find-message-at-point (pos)
  (get-char-property pos 'keybase-message-id))

(defun keybase--handle-post-message (json)
  (let ((id (keybase--json-find json '(id)))
        (message (keybase--json-find json '(content text body)))
        (sender (keybase--json-find json '(sender username)))
        (timestamp (keybase--json-find json '(sent_at))))
    (keybase--insert-message id timestamp sender message)))

(defun keybase--delete-message-by-pos (pos)
  (destructuring-bind (start end)
      pos
    (let ((inhibit-read-only t))
      (delete-region start end))))

(defun keybase--delete-message (id)
  (let ((old-message-pos (keybase--find-message-in-log id)))
    (when old-message-pos
      (keybase--delete-message-by-pos old-message-pos))))

(defun keybase--handle-delete (json)
  (let ((message-list (keybase--json-find json '(content delete messageIDs))))
    (loop for id across message-list
          do (keybase--delete-message id))))

(defun keybase--handle-edit (json)
  (let* ((old-msgid (keybase--json-find json '(content edit messageID)))
         (old-message-pos (keybase--find-message-in-log old-msgid)))
    ;; If the message isn't already displayed, we don't need to do
    ;; anything (we don't want old messages added just because someone
    ;; edited them)
    (when old-message-pos
      (save-excursion
        (let* ((msg (car old-message-pos))
               (old-timestamp (get-char-property msg 'keybase-timestamp)))
          (unless old-timestamp
            (error "no timestamp for previous message"))
          (keybase--delete-message-by-pos old-message-pos)
          ;; An UPDATE message contains the same fields as a TEXT message.
          (let ((message (keybase--json-find json '(content edit body)))
                (sender (keybase--json-find json '(sender username))))
            (keybase--insert-message-content msg old-msgid old-timestamp sender message)))))))

(defun keybase--handle-image-message (json)
  )

(cl-defun keybase--handle-incoming-chat-message (json)
  (let ((msg (keybase--json-find json '(msg) :error-if-missing nil)))
    (when msg
      (let* ((channel-info (keybase--parse-channel-name (keybase--json-find msg '(channel))))
             (buffer (keybase--find-channel-buffer channel-info :if-missing :ignore)))
        (when buffer
          (with-current-buffer buffer
            (let ((type (keybase--json-find msg '(content type))))
              (cond ((equal type "text")
                     (keybase--handle-post-message msg))
                    ((equal type "delete")
                     (keybase--handle-delete msg))
                    ((equal type "edit")
                     (keybase--handle-edit msg))
                    ((equal type "attachment")
                     (keybase--handle-image-message msg))))))))))

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

(defun keybase--channel-info-as-json (channel-info)
  (append (if (first channel-info) `((members_type . ,(first channel-info))) nil)
          (if (third channel-info) `((topic_name . ,(third channel-info))) nil)
          `((name . ,(second channel-info))
            (topic_type . "chat"))))

(defun keybase--parse-channel-name (json)
  (let ((members-type (keybase--json-find json '(members_type) :error-if-missing nil))
        (name         (keybase--json-find json '(name)))
        (topic-name   (keybase--json-find json '(topic_name)   :error-if-missing nil)))
    (list members-type name topic-name)))

(defun keybase--list-channels ()
  (let ((result (keybase--request-api '((method . "list")))))
    (loop for conversation across (keybase--json-find result '(result conversations))
          collect (keybase--parse-channel-name (keybase--json-find conversation '(channel))))))

(defun keybase--input (str)
  (unless keybase--channel-info
    (error "No channel info available in this buffer"))
  (keybase--request-api `((method . "send")
                          (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                 (message . ((body . ,str))))))))))

(defun keybase-send-input-line ()
  "Send the currently typed line to the server."
  (interactive)
  (let ((text (string-trim (keybase--read-input-line keybase--input-marker (point-max)))))
    (when (not (equal text ""))
      (delete-region keybase--input-marker (point-max))
      (keybase--input text))))

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
                 (keybase--handle-incoming-chat-message (json-read-from-string content))
                 (setq pos nl)))
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
              for topic-name = (third channel)
              for name = (if topic-name
                             (format "%s/%s" (second channel) (third channel))
                           (second channel))
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
  (interactive (list (keybase--choose-channel-info)))
  (let ((buf (keybase--find-channel-buffer channel-info :if-missing :create)))
    (switch-to-buffer buf)))

(provide 'keybase)
