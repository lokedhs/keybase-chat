;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)
(require 'notifications)
(require 'cl)

(defgroup keybase nil
  "Keybase chat implementation"
  :prefix 'keybase
  :group 'applications)

(defcustom keybase--program "keybase"
  "The name of the keybase binary"
  :type 'string
  :group 'keybase)

(defcustom keybase-attribution 'keybase-default-attribution
  "A function that prints the attribution before each Keybase message.
It will be given two arguments, the timestamp of the message in seconds since
the epoch and the sender's keybase name."
  :type 'function
  :group 'keybase)

(defcustom keybase-channel-mode-hook nil
  "Hook called by `keybase-channel-mode'"
  :type 'hook
  :group 'keybase)

(defface keybase-default
  ()
  "Default face for chat buffers."
  :group 'keybase)

(defface keybase-message-text-content
  '((t
     :inherit keybase-default))
  "Face used to display the text content of messages."
  :group 'keybase)

(defface keybase-message-text-content-in-progress
  '((((class color))
     :foreground "#505050"
     :inherit keybase-default)
    (t
     :inherit keybase-default))
  "Face used to display the text content of a message that has
not been confirmed from the server yet.")

(defface keybase-message-from
  '((((class color))
     :foreground "#00b000"
     :inherit keybase-default)
    (t
     :inherit keybase-default))
  "Face used to display the 'from' part of a message."
  :group 'keybase)

(defface keybase-channel-summary-title
  '((t
     :weight bold
     :inherit keybase-default))
  "Face used to display the headlines in the channel list."
  :group 'keybase)

(defface keybase-channel-summary-team
  '((t
     :weight bold
     :inherit keybase-default))
  "Face used to display the team name in the channel list"
  :group 'keybase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(cl-defmacro keybase--with-json-bind ((&rest defs) json &body body)
  (declare (indent 2))
  (let ((json-sym (gensym "json")))
    `(let ((,json-sym ,json))
       (let ,(loop for (sym path) in defs
                   collect `(,sym (keybase--json-find ,json-sym ',path)))
         ,@body))))

(cl-defmacro keybase--ensure-hash-value (key hash &body body)
  (let ((hash-sym (gensym))
        (key-sym (gensym))
        (val-sym (gensym))
        (default-value-sym (gensym)))
    `(let* ((,hash-sym ,hash)
            (,key-sym ,key)
            (,val-sym (gethash ,key-sym ,hash-sym ',default-value-sym)))
       (if (eq ,val-sym ',default-value-sym)
           (let ((,val-sym (progn ,@body)))
             (setf (gethash ,key-sym ,hash-sym) ,val-sym)
             ,val-sym)
         ;; ELSE: The value was found in the hash table
         ,val-sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase-channel-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'keybase-open-selected-channel)
    (define-key map (kbd "RET") 'keybase-open-selected-channel)
    map))

(defun keybase--make-channel-button (name channel)
  (propertize name
              'font-lock-face 'link
              'mouse-face 'highlight
              'help-echo (format "mouse-2: open channel buffer")
              'keybase-channel-name channel
              'keymap keybase-channel-link-keymap))

(defun keybase-open-selected-channel ()
  (interactive)
  (when-let ((channel (get-char-property (point) 'keybase-channel-name)))
    (keybase-join-channel channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase--proc-buf nil)
(defvar keybase--active-buffers nil
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
          (keybase--request-chat-api `((method . "delete")
                                       (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                              (message_id . ,msgid))))))))
      ;; ELSE: No message at point
      (message "No message at point"))))

(defun keybase--buffer-closed ()
  (setq keybase--active-buffers (cl-remove (current-buffer) keybase--active-buffers :key #'cdr :test #'eq)))

(defun keybase--generate-channel-name (channel-info)
  (if (third channel-info)
      (format "*keybase %s - %s*" (second channel-info) (third channel-info))
    (format "*keybase %s*" (second channel-info))))

(defun keybase--window-config-updated ()
  "Hook function that is locally installed for window-configuration-change-hook in all channel buffers."
  (let ((recompute nil))
    (when (get-buffer-window)
      ;; Clear unread count
      (when (plusp keybase--unread-in-channel)
        (setq recompute t)))
    (when recompute
      (keybase--recompute-modeline))))

(defun keybase--create-buffer (channel-info)
  ;; First ensure that the listener is running
  (keybase--find-process-buffer)
  ;; Create the buffer
  (let ((buffer (generate-new-buffer (keybase--generate-channel-name channel-info))))
    (with-current-buffer buffer
      (keybase-channel-mode)
      (setq-local keybase--channel-info channel-info)
      (setq-local keybase--unread-in-channel 0)
      (add-hook 'kill-buffer-hook 'keybase--buffer-closed nil t)
      (push (cons channel-info buffer) keybase--active-buffers)
      (keybase--load-initial-messages))
    (unless (member 'keybase-display-notifications-string global-mode-string)
      (if global-mode-string
          (setq global-mode-string (append global-mode-string '(keybase-display-notifications-string)))
        (setq global-mode-string '("" keybase-display-notifications-string)))
      (keybase--recompute-modeline))
    buffer))

(cl-defun keybase--find-channel-buffer (channel-info &key (if-missing :error))
  (unless (member if-missing '(:error :create :ignore))
    (error "Illegal argument to if-missing: %S" if-missing))
  (let ((e (find channel-info keybase--active-buffers :key #'car :test #'equal)))
    (cond (e
           (cdr e))
          ((eq if-missing :create)
           (keybase--create-buffer channel-info))
          ((eq if-missing :error)
           (error "No buffer for channel %S" channel-info)))))

(defun keybase--load-initial-messages ()
  (let* ((messages-json (keybase--request-chat-api `((method . "read")
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
          do (keybase--insert-message id timestamp sender (keybase--json-find content '(text body)) nil))))

(defun keybase--format-date (timestamp)
  (let ((time (seconds-to-time timestamp)))
    (format-time-string "%Y-%m-%d %H:%M:%S" time)))

(defun keybase--recompute-modeline ()
  (setq keybase-display-notifications-string (keybase--make-unread-notification-string))
  (force-mode-line-update t))

(defun keybase--make-unread-notification-string ()
  (with-output-to-string
    (princ "Unread: ")
    (let ((unread-channels (loop with first = t
                                 for channel in keybase--active-buffers
                                 for (name unread) = (with-current-buffer (cdr channel)
                                                       (list keybase--channel-info keybase--unread-in-channel))
                                 when (plusp unread)
                                 collect name)))
      (if unread-channels
          (loop for first = t then nil
                for name in unread-channels
                unless first
                do (princ ", ")
                do (princ (keybase--generate-channel-name name)))
        ;; ELSE: Just return the empty string
        ""))))

(defun keybase-default-attribution (sender timestamp)
  (format "[%s] %s " sender (keybase--format-date timestamp)))

(defvar keybase--current-id 0)

(defun keybase--make-in-progress-id ()
  (format "temp-%d" (incf keybase--current-id)))

(defun keybase--insert-message-content (pos id timestamp sender message image)
  "Insert message content at position POS.
ID may be nil, in which case this message represents an
in-progress message which is inserted while a new message is
being inserted. It will later be replaced with the real content
once it is received from the server."
  (goto-char pos)
  (let ((inhibit-read-only t))
    (let ((start (point)))
      (insert (propertize (funcall keybase-attribution sender timestamp)
                          'face 'keybase-message-from))
      (let ((text-start (point)))
       (when (> (length message) 0)
         (insert (concat message "\n\n")))
       (when image
         (destructuring-bind (image-title image-filename)
             image
           (keybase--insert-image image-title image-filename)
           (insert "\n\n")))
       (let ((gen-id (or id (keybase--make-in-progress-id))))
         (add-text-properties start (point)
                              (append (list 'read-only t
                                            'keybase-message-id gen-id
                                            'keybase-timestamp timestamp
                                            'keybase-sender sender
                                            'front-sticky '(read-only))
                                      (if (null id)
                                          (list 'keybase-in-progress gen-id
                                                'keybase-content message)
                                        nil)))
         (add-text-properties text-start (point)
                              (list 'face (if id 'keybase-message-text-content 'keybase-message-text-content-in-progress))))))))

(defun keybase--insert-message (id timestamp sender message image)
  (save-excursion
    (goto-char keybase--output-marker)
    (let ((new-pos (loop with prev-pos = (point)
                         for pos = (previous-single-char-property-change prev-pos 'keybase-timestamp)
                         until (let ((prop (get-char-property pos 'keybase-timestamp)))
                                 (and prop (< prop timestamp)))
                         do (setq prev-pos pos)
                         until (= pos (point-min))
                         finally (return prev-pos))))
      (keybase--insert-message-content new-pos id timestamp sender message image))))

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
  (let ((id        (keybase--json-find json '(id)))
        (message   (keybase--json-find json '(content text body)))
        (sender    (keybase--json-find json '(sender username)))
        (timestamp (keybase--json-find json '(sent_at))))
    ;; If this message is sent by us, we need to check if there is an
    ;; in-progress message inserted in the buffer. If so, it beeds to
    ;; be removed before the real one is sent.
    (when (equal sender (with-current-buffer keybase--proc-buf keybase--username))
      (save-excursion
        (loop with curr = (point-min)
              for pos = (next-single-property-change curr 'keybase-in-progress)
              while pos
              when (and (get-char-property pos 'keybase-in-progress)
                        (equal (get-char-property pos 'keybase-content) message))
              do (let ((end (next-single-property-change pos 'keybase-message-id))
                       (inhibit-read-only t))
                   (delete-region pos end)
                   (return nil))
              do (setq curr pos))))
    (keybase--insert-message id timestamp sender message nil)
    (let ((old keybase--unread-in-channel))
      (incf keybase--unread-in-channel)
      (when (zerop old)
        (keybase--recompute-modeline)))))

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
            (keybase--insert-message-content msg old-msgid old-timestamp sender message nil)))))))

(defvar *keybase--attachment-type-none* 0)
(defvar *keybase--attachment-type-image* 1)
(defvar *keybase--attachment-type-video* 2)
(defvar *keybase--attachment-type-audio* 3)

(defun keybase--file-to-extension (file)
  "Returns the extension for the given FILE, or null if the file does not have an extension."
  (let ((result (string-match "^.*\\.\\([^.]+\\)$" file)))
    (if result
        (match-string 1 file)
      nil)))

(defun keybase--insert-image (title filename)
  (let ((image-data (with-temp-buffer
                      (insert-file-contents-literally filename)
                      (decode-coding-string (buffer-string) 'no-conversion))))
    (let ((image (create-image image-data nil t)))
      (insert-image image "[image]"))))

(defun keybase--handle-image-message (json)
  (let* ((id         (keybase--json-find json '(id)))
         (sender     (keybase--json-find json '(sender username)))
         (timestamp  (keybase--json-find json '(sent_at)))
         (attachment (keybase--json-find json '(content attachment)))
         (asset-type (keybase--json-find attachment '(object metadata assetType))))
    (when (eql asset-type *keybase--attachment-type-image*)
      (let* ((filename     (keybase--json-find attachment '(object filename)))
             (content-type (keybase--json-find attachment '(object mimeType)))
             (title        (keybase--json-find attachment '(object title)))
             (file (make-temp-file "emacs-keybase" nil (format ".%s" (keybase--file-to-extension filename)))))
        (unwind-protect
            (progn
              (keybase--request-chat-api `((method . "download")
                                           (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                                  (message_id . ,id)
                                                                  (output . ,file)))))))
              (keybase--insert-message id timestamp sender "Uploaded file" (list title file)))
          (delete-file file))))))

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
                     (keybase--handle-image-message msg))))))
        ;; We need to check mentions for all channels, not just the ones the user have opened
        (let ((at-mention-usernames (keybase--json-find msg '(at_mention_usernames) :error-if-missing nil))
              (username (with-current-buffer keybase--proc-buf keybase--username)))
          (when (loop for v across at-mention-usernames
                      when (equal v username)
                      return t)
            (message "mention in channel: %S" channel-info)))))))

(defun keybase--request-api (command command-args arg)
  (let ((output-buf (generate-new-buffer " *keybase api*")))
    (unwind-protect
        (progn
          (if arg
              (with-temp-buffer
                (insert (json-encode arg))
                (apply #'call-process-region (point-min) (point-max) command nil (list output-buf nil) nil command-args))
            (apply #'call-process command nil (list output-buf nil) nil command-args))
          (with-current-buffer output-buf
            (goto-char (point-min))
            (json-read)))
      (kill-buffer output-buf))))

(defun keybase--request-api-async (command command-args arg callback)
  (let ((output-buf (generate-new-buffer " *keybase api*")))
    (let ((proc (make-process :name "keybase-api"
                              :command (cons command command-args)
                              :buffer output-buf
                              :stderr " *keybase api error*"
                              :coding 'utf-8
                              :sentinel (lambda (proc type)
                                          (when (string-match "^\\(finished\\|deleted\\|exited\\|failed\\)" type)
                                            (unwind-protect
                                                (when (string-match "^finished" type)
                                                  (with-current-buffer output-buf
                                                    (goto-char (point-min))
                                                    (let ((result (json-read)))
                                                      (funcall callback result))))
                                              (kill-buffer output-buf)))))))
      (when arg
        (let ((encoded (json-encode arg)))
          (process-send-string proc encoded))
        (process-send-eof proc)))))

(defun keybase--request-chat-api (arg)
  (keybase--request-api keybase--program '("chat" "api") arg))

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
  (let ((result (keybase--request-chat-api '((method . "list")))))
    (loop for conversation across (keybase--json-find result '(result conversations))
          for channel-name = (keybase--parse-channel-name (keybase--json-find conversation '(channel)))
          for unread = (not (eq (keybase--json-find conversation '(unread)) :false))
          collect (list channel-name unread))))

(defun keybase--input (str)
  (unless keybase--channel-info
    (error "No channel info available in this buffer"))
  (save-excursion
    (goto-char keybase--output-marker)
    (keybase--insert-message-content (point)
                                     nil
                                     (time-to-seconds (current-time))
                                     (with-current-buffer keybase--proc-buf keybase--username)
                                     str
                                     nil))
  (keybase--request-api-async keybase--program
                              (list "chat" "api")
                              `((method . "send")
                                (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                       (message . ((body . ,str))))))))
                              (lambda (json)
                                nil)))

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
		 (condition-case err
		     (progn
		       (keybase--handle-incoming-chat-message (json-read-from-string content))
		       (setq pos nl))
		   (json-readtable-error (message "ate bad json") (setq pos nl)))))
      (delete-region (point-min) (point)))))

(defun keybase--connect-to-server ()
  (let ((name " *keybase server*"))
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
          (let ((json (keybase--request-api keybase--program '("status" "--json") nil)))
            (setq-local keybase--server-process proc)
            (setq-local keybase--channels nil)
            (setq-local keybase--username (keybase--json-find json '(Username)))))
        (setq keybase--proc-buf buf)
        buf))))

(defun keybase--find-active-process-buffer ()
  (when keybase--proc-buf
    (if (buffer-live-p keybase--proc-buf)
        (with-current-buffer keybase--proc-buf
          (if (process-live-p keybase--server-process)
              keybase--proc-buf
            (progn
              (kill-buffer keybase--proc-buf)
              (setq keybase--proc-buf nil)
              nil)))
      (progn
        (setq keybase--proc-buf nil)
        nil))))

(defun keybase--find-process-buffer ()
  (let ((buf (keybase--find-active-process-buffer)))
    (or buf (keybase--connect-to-server))))

(defun keybase--disconnect-from-server ()
  (let ((buf (keybase--find-active-process-buffer)))
    (when buf
      (kill-buffer buf)
      (setq keybase--proc-buf nil))))

(defun keybase--choose-channel-info ()
  (let ((channels (keybase--list-channels)))
    (destructuring-bind (names-list names-ref)
        (loop for e in channels
              for channel = (first e)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase-conversations-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'keybase-conversations-refresh)
    map))

(define-derived-mode keybase-conversations-list-mode fundamental-mode "Keybase conversations"
  "Major mode for displaying the keymap help."
  (use-local-map keybase-conversations-list-mode-map)
  (read-only-mode 1)
  (setq truncate-lines t))

(defun keybase--sort-team-list (channels)
  (let ((team-list (make-hash-table :test 'equal)))
    (loop for channel in channels
          for channel-name = (first channel)
          when (equal (first channel-name) "team")
          do (let* ((team-name (second channel-name))
                    (name (third channel-name))
                    (v (gethash team-name team-list :unset)))
               (if (eq v :unset)
                   (setf (gethash team-name team-list) (list channel))
                 (setf (gethash team-name team-list) (cons channel v)))))
    team-list))

(defun keybase--render-team-list (channels)
  (let ((team-list (keybase--sort-team-list channels)))
    (insert (propertize "Teams\n" 'face 'keybase-channel-summary-title))
    (let ((team-names (sort (loop for name being each hash-key in team-list collect name) #'string<)))
      (loop for name in team-names
            for channel-list = (gethash name team-list)
            do (progn
                 (insert "\n  ")
                 (insert (propertize name 'face 'keybase-channel-summary-team))
                 (insert "\n")
                 (loop for (channel unread) in (sort channel-list (lambda (a b) (string< (third (car a)) (third (car b)))))
                       do (progn
                            (insert "    ")
                            (insert (keybase--make-channel-button (third channel) channel))
                            (when unread
                              (insert " (unread)"))
                            (insert "\n"))))))))

(defun keybase--render-private-list (channels)
  (insert "\n")
  (insert (propertize "Private conversations\n\n" 'face 'keybase-channel-summary-title))
  (loop for (channel-name unread) in channels
        when (equal (first channel-name) "impteamnative")
        do (progn
             (insert (second channel-name))
             (insert "\n"))))

(defun keybase-conversations-refresh ()
  (interactive)
  (unless (eq major-mode 'keybase-conversations-list-mode)
    (error "Buffer is not a keybase-conversations-list"))
  (let ((buffer (current-buffer)))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert "Loading conversations...\n")
      (let ((overlay (make-overlay (point-min) (point-max))))
        (keybase--request-api-async keybase--program '("chat" "api") '((method . "list"))
                                    (lambda (json)
                                      (with-current-buffer buffer
                                        (let ((inhibit-read-only t)
                                              (start (overlay-start overlay))
                                              (end (overlay-end overlay)))
                                          (goto-char start)
                                          (delete-region start end)
                                          (delete-overlay overlay)
                                          (let ((channels (keybase--list-channels))
                                                (team-list (make-hash-table :test 'equal))
                                                (private-list (make-hash-table :test 'equal)))
                                            (keybase--render-team-list channels)
                                            (keybase--render-private-list channels))))))))))

(defun keybase-list-conversations ()
  (interactive)
  (let ((buffer (get-buffer "*keybase conversations*")))
    (when (and buffer (not (eq (with-current-buffer buffer major-mode) 'keybase-conversations-list-mode)))
      (error "Conversation lists buffer already exists but has the wrong mode"))
    (unless buffer
      (setq buffer (get-buffer-create "*keybase conversations*")))
    (with-current-buffer buffer
      (keybase-conversations-list-mode)
      (keybase-conversations-refresh))
    (pop-to-buffer buffer)))

(provide 'keybase)
