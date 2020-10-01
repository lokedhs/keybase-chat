;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)
(require 'notifications)
(require 'cl)
(require 'keybase-markup)

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

(defface keybase-message-text-content-bold
  '((t
     :weight bold
     :inherit keybase-default))
  "Face used to display bold text."
  :group 'keybase)

(defface keybase-message-text-content-italics
  '((t
     :slant italic
     :foreground "#00ff00"
     :inherit keybase-default))
  "Face used to display italics text."
  :group 'keybase)

(defface keybase-message-text-content-code
  '((((class color))
     :background "#f0f0f0"
     :inherit keybase-default)
    (t
     :inherit keybase-default))
  "Face used to display code snippets."
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

(defun keybase--json-parse-result-buffer ()
  (let* ((content (buffer-substring (point) (point-max)))
         (decoded-content (decode-coding-string content 'utf-8)))
    (json-read-from-string decoded-content)))

(defun keybase--url-handler (status buffer callback as-json-p)
  (let ((error-status (getf status :error)))
    (if error-status
        (progn
          (message "Got error: %S" status)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer (current-buffer)))
          (signal (car error-status) (cdr error-status)))
      ;; ELSE: No error
      (progn
        (goto-char (point-min))
        (search-forward "\n\n")
        (let ((data (if as-json-p
                        (potato--keybase-parse-result-buffer)
                      (buffer-substring (point) (point-max)))))
          (with-current-buffer buffer
            (funcall callback data))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer (current-buffer))))))))

(cl-defun keybase--url-retrieve (url method callback &key (as-json-p t) ignore-response)
  (let ((buffer (current-buffer)))
    (let ((url-request-method method))
      (url-retrieve url
                    (lambda (status)
                      (unless ignore-response
                        (keybase--url-handler status buffer callback as-json-p)))
                    nil t))))

(defun keybase--insert-image-handler (overlay data)
  (let ((image (create-image data nil t))
        (start (overlay-start overlay))
        (end (overlay-end overlay)))
    (delete-overlay overlay)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char start)
        (delete-region start end)
        (let ((start (point)))
          (insert-image image "[image]"))))))

(defun keybase--insert-image-url-async (url)
  "Downloads and insert the image specified by URL at point.
The download is performed in the background, and while
downloading the image, a temporary message is displayed. This
message is then replaced by the image once the download has
finished."
  (let ((start (point)))
    (insert "[loading-image]")
    (let ((overlay (make-overlay start (point))))
      (keybase--url-retrieve url "GET"
                             (lambda (data)
                               (keybase--insert-image-handler overlay data))
                             :as-json-p nil))))

(defun keybase--find-or-make-empty-buffer (name initialiser)
  (let ((buffer (get-buffer name)))
    (if buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))
            buffer))
      (let ((buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (funcall initialiser))
        buffer))))

(defvar keybase-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'keybase-open-selected-button)
    (define-key map (kbd "RET") 'keybase-open-selected-button)
    map))

(defun keybase-open-selected-button ()
  (interactive)
  (let ((callback (get-char-property (point) 'button-function))
        (data (get-char-property (point) 'button-data)))
    (funcall callback data)))

(cl-defun keybase--make-clickable-button (message function data)
  (propertize message
              'font-lock-face 'link
              'keymap keybase-button-keymap
              'mouse-face 'highlight
              'button-function function
              'button-data data))

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

(defvar keybase-username-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'keybase-open-selected-username)
    (define-key map (kbd "RET") 'keybase-open-selected-username)
    map))

(defun keybase-open-selected-username ()
  (interactive)
  (when-let ((user (get-char-property (point) 'keybase-user)))
    (keybase-user-info user)))

(cl-defun keybase--make-clickable-username (name &key include-prefix highlight)
  (apply #'propertize (format "%s%s" (if include-prefix "@" "") name)
         'font-lock-face 'link
         'help-echo (format "mouse-2: open user info for %s" name)
         'keybase-user name
         'keymap keybase-username-link-keymap
         (if highlight
             (list 'mouse-face 'highlight)
           nil)))

(defun keybase--private-conversation-channel-name (user)
  (let ((current-name (with-current-buffer keybase--proc-buf keybase--username)))
    (list "impteamnative" (if (string< current-name user)
                              (format "%s,%s" current-name user)
                            (format "%s,%s" user current-name))
          nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase--proc-buf nil)
(defvar keybase--active-buffers nil
  "List of active channels.
Each entry is of the form (CHANNEL-INFO . BUFFER)")
(defvar keybase--channels nil
  "List of channels.
Each entry is of the form (CHANNEL-INFO UNREAD")

(defvar keybase-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'keybase-insert-nl)
    (define-key map (kbd "RET") 'keybase-send-input-line)
    ;;(define-key map (kbd "@") 'keybase-insert-user)
    (define-key map (kbd "C-c C-d") 'keybase-delete-message)
    (define-key map (kbd "C-c C-r") 'keybase-reply-to-message)
    (define-key map [menu-bar keybase] (cons "Keybase" (make-sparse-keymap "Keybase")))
    (define-key map [menu-bar keybase join-channel] '("Join channel" . keybase-join-channel))
    (define-key map [menu-bar keybase create-private-conversation] '("Private conversation" . keybase-create-private-converstion))
    (define-key map [menu-bar keybase show-user-info] '("User info" . keybase-user-info))
    (define-key map [menu-bar keybase delete-message] '("Delete message" . keybase-delete-message))
    map))

(defun keybase-reply-to-message ()
  (interactive)
  (let ((reply-to-msgid (keybase--find-message-at-point (point)))
        (sender (get-char-property (point) 'keybase-sender)))
    (if reply-to-msgid
        (keybase--input (read-from-minibuffer "Reply: " ) reply-to-msgid)
      (message "No message at point"))))

(defun keybase--load-more-messages-handler (data)
  (keybase-load-messages))

(define-derived-mode keybase-channel-mode nil "Keybase"
  "Mode for Keybase channel content"
  (use-local-map keybase-channel-mode-map)
  (insert (keybase--make-clickable-button "[Load more messages]" #'keybase--load-more-messages-handler nil))
  (insert "\n\n")
  (setq-local keybase--start-of-messages-marker (make-marker))
  (set-marker keybase--start-of-messages-marker (point))
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

(defvar keybase--mark-unread-in-progress nil)
(defvar keybase--mark-unread-pending nil)

(defun keybase--find-most-recent-message ()
  (let ((pos (previous-single-char-property-change (point-max) 'keybase-remote-message-id)))
    (if pos
        (get-char-property (1- pos) 'keybase-remote-message-id)
      nil)))

(defun keybase--mark-unread-start-process ()
  (when keybase--mark-unread-in-progress
    (error "Mark unread already in progress"))
  (if-let ((msgid (keybase--find-most-recent-message)))
      (let ((proc (keybase--request-api-async keybase--program
                                              '("chat" "api")
                                              `((method . "mark")
                                                (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info)))))))
                                              (lambda (json)
                                                (setq keybase--mark-unread-in-progress nil)
                                                (keybase--mark-pending-unread)))))
        (setq keybase--mark-unread-in-progress proc)
        t)
    ;; ELSE: Return nil to indicate that a process was not started
    nil))

(defun keybase--mark-pending-unread ()
  (loop while keybase--mark-unread-pending
        until (let ((req (car keybase--mark-unread-pending)))
                (setq keybase--mark-unread-pending (cdr keybase--mark-unread-pending))
                (with-current-buffer req
                  (keybase--mark-unread-start-process)))))

(defun keybase--mark-unread ()
  (when (plusp keybase--unread-in-channel)
    (setq keybase--unread-in-channel 0)
    (if keybase--mark-unread-in-progress
        (cl-pushnew (current-buffer) keybase--mark-unread-pending)
      (keybase--mark-unread-start-process))))

(defun keybase--process-buffer-list-update ()
  (unless (eq major-mode 'keybase-channel-mode)
    (error "This function should only be called with channel buffers"))
  (when (eq (current-buffer) (car (buffer-list)))
    (keybase--mark-unread)))

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
      (add-hook 'buffer-list-update-hook 'keybase--process-buffer-list-update nil t)
      (push (cons channel-info buffer) keybase--active-buffers)
      (setq-local keybase--next-tag nil)
      (keybase-load-messages 10))
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

(cl-defun keybase-load-messages (&optional (num 10))
  "Load NUM messages from the message history."
  (interactive)
  (let* ((messages-json (keybase--request-chat-api `((method . "read")
                                                     (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                                            (pagination . (,@(if keybase--next-tag
                                                                                                 `((next . ,keybase--next-tag))
                                                                                               nil)
                                                                                           (num . ,num))))))))))
         (next-tag (keybase--json-find messages-json '(result pagination next)))
         (messages (keybase--json-find messages-json '(result messages))))
    (setq keybase--next-tag next-tag)
    (loop for msg-entry across messages
          for msg = (keybase--json-find msg-entry '(msg))
          for id = (keybase--json-find msg '(id))
          for sender = (keybase--json-find msg '(sender username))
          for timestamp = (keybase--json-find msg '(sent_at_ms))
          for content = (keybase--json-find msg '(content))
          for type = (keybase--json-find content '(type))
          when (equal type "text")
          do (keybase--insert-message id timestamp sender (keybase--json-find content '(text body)) nil))))

(defun keybase--format-date (timestamp)
  (let ((time (seconds-to-time (/ timestamp 1000))))
    (format-time-string "%Y-%m-%d %H:%M:%S" time)))

(defun keybase--format-simple-date (timestamp)
  (let ((time (seconds-to-time (/ timestamp 1000))))
    (format-time-string "%H:%M" time)))

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
  (format "[%s] %s "
          (keybase--make-clickable-username sender :highlight nil)
          (keybase--format-simple-date timestamp)))

(defvar keybase--first-paragraph nil)

(defun keybase--render-markup-element (element)
  (etypecase element
    (string (insert element))
    (list (if (stringp (car element))
              (insert (car element))
            (ecase (car element)
              (:paragraph
               (let ((col (current-column))
                     (wrapcol fill-column))
                 (let ((content (with-temp-buffer
                                  (keybase--insert-markup-inner (cdr element))
                                  (let ((sentence-end-double-space nil)
                                        (fill-column (- wrapcol col)))
                                    (fill-region (point-min) (point-max))
                                    (buffer-string)))))
                   (loop for v in (keybase--split-with-regexp "\n" content :empty t)
                         for first = t then nil
                         unless first
                         do (loop repeat col
                                  do (insert " "))
                         do (progn
                              (insert v)
                              (insert "\n"))))))
              (:newline
               (insert "\n"))
              (:bold
               (let ((start (point)))
                 (keybase--insert-markup-inner (cdr element))
                 (add-text-properties start (point) (list 'face 'keybase-message-text-content-bold))))
              (:italics
               (let ((start (point)))
                 (keybase--insert-markup-inner (cdr element))
                 (add-text-properties start (point) (list 'face 'keybase-message-text-content-italics))))
              (:code
               (insert (propertize (cdr element) 'face 'keybase-message-text-content-code)))
              (:code-block
               (insert "\n")
               (insert (propertize (third element) 'face 'keybase-message-text-content-code))
               (insert "\n"))
              (:user
               (insert (keybase--make-clickable-username (cdr element) :include-prefix t :highlight t))))))))

(defun keybase--insert-markup-inner (content)
  (loop for v in content
        do (keybase--render-markup-element v)))

(defun keybase--parse-user (string fn)
  (let ((pos 0)
        (length (length string))
        (result nil))
    (cl-labels ((append-res (elems) (setq result (append result elems)))
                (collect-part (v) (when (< pos v) (setq result (append result (funcall fn (subseq string pos v)))))))
      (loop while (< pos length)
            do (let ((result (string-match "@\\([a-z0-9_]+\\)\\(?:$\\|\\W\\)" string pos)))
                 (if result
                     (let ((user (match-string 1 string))
                           (end-pos (match-end 1)))
                       (collect-part result)
                       (append-res `((:user . ,user)))
                       (setq pos end-pos))
                   ;; ELSE: No more results
                   (collect-part length)
                   (setq pos length))))
      result)))

(defun keybase--insert-markup-string (message)
  (let ((content (let ((keybase--custom-parser-3 #'keybase--parse-user))
                   (keybase--markup-paragraphs message :allow-nl t))))
    (let ((keybase--first-paragraph t))
      (keybase--insert-markup-inner content))))

(defvar keybase--current-id 0)

(defun keybase--make-in-progress-id ()
  (format "temp-%d" (incf keybase--current-id)))

(defun keybase--insert-message-content (id timestamp sender message image)
  "Insert message content at the current cursor position.
ID may be nil, in which case this message represents an
in-progress message which is inserted while a new message is
being inserted. It will later be replaced with the real content
once it is received from the server."
  (let ((inhibit-read-only t)
        (start (point)))
    (insert (propertize (funcall keybase-attribution sender timestamp)
                          'face 'keybase-message-from))
      (let ((text-start (point)))
        (when (> (length message) 0)
          (keybase--insert-markup-string message)
          (insert "\n"))
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
                                                 'keybase-content message
                                                 'face 'keybase-message-text-content-in-progress)
                                         (list 'keybase-remote-message-id id))))))))

(defun keybase--insert-message (id timestamp sender message image)
  (save-excursion
    (goto-char keybase--output-marker)
    (let ((new-pos (loop with prev-pos = (point)
                         for pos = (previous-single-char-property-change prev-pos 'keybase-timestamp)
                         do (let ((prop (get-char-property pos 'keybase-timestamp)))
                              (when (and prop (< prop timestamp))
                                (return prev-pos)))
                         do (when (<= pos (marker-position keybase--start-of-messages-marker))
                              (return (marker-position keybase--start-of-messages-marker)))
                         do (setq prev-pos pos)
                         finally (return prev-pos))))
      (goto-char new-pos)
      (keybase--insert-message-content id timestamp sender message image))))

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
        (timestamp (keybase--json-find json '(sent_at_ms))))
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

(defun keybase--delete-message (id)
  (message "deleting message %S" id)
  (let ((old-message-pos (keybase--find-message-in-log id)))
    (when old-message-pos
      (let ((inhibit-read-only t))
        (delete-region (first old-message-pos) (second old-message-pos))))))

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
      (destructuring-bind (old-message-start old-message-end)
          old-message-pos
        (save-excursion
          (let* ((msg old-message-start)
                 (old-timestamp (get-char-property msg 'keybase-timestamp)))
            (unless old-timestamp
              (error "no timestamp for previous message"))
            (let ((inhibit-read-only t))
              (delete-region old-message-start old-message-end))
            ;; An UPDATE message contains the same fields as a TEXT message.
            (let ((message (keybase--json-find json '(content edit body)))
                  (sender (keybase--json-find json '(sender username))))
              (goto-char old-message-start)
              (keybase--insert-message-content msg old-timestamp sender message nil))))))))

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

(cl-defun keybase--request-api-async (command command-args arg callback &key buffer)
  (let ((output-buf (generate-new-buffer " *keybase api*")))
    (let ((proc (make-process :name "keybase-api"
                              :command (cons command command-args)
                              :buffer (or buffer output-buf)
                              :stderr " *keybase api error*"
                              :coding 'utf-8
                              :filter (lambda (proc s) (with-current-buffer output-buf (insert s)))
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
        (process-send-eof proc)
        proc))))

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

(defun keybase--list-channels (&optional force-reload)
  (if (and keybase--channels (not force-reload))
      keybase--channels
    (keybase--reload-channels)))

(defun keybase--reload-channels ()
  (let ((result (keybase--request-chat-api '((method . "list")))))
    (let ((channels (loop for conversation across (keybase--json-find result '(result conversations))
                          for channel-name = (keybase--parse-channel-name (keybase--json-find conversation '(channel)))
                          for unread = (not (eq (keybase--json-find conversation '(unread)) :json-false))
                          collect (list channel-name unread))))
      (setq keybase--channels channels)
      channels)))

(defun keybase--input (str &optional reply-to-msgid)
  (unless keybase--channel-info
    (error "No channel info available in this buffer"))
  (save-excursion
    (goto-char keybase--output-marker)
    (keybase--insert-message-content nil
                                     (time-to-seconds (current-time))
                                     (with-current-buffer keybase--proc-buf keybase--username)
                                     str
                                     nil))
  (if reply-to-msgid (keybase--request-api-async keybase--program
                              (list "chat" "api")
                              `((method . "send")
                                (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                       (message . ((body . ,str)))
                                                       (reply_to . ,reply-to-msgid))))))
                              (lambda (json)
                                nil))
    (keybase--request-api-async keybase--program
                              (list "chat" "api")
                              `((method . "send")
                                (params . ((options . ((channel . ,(keybase--channel-info-as-json keybase--channel-info))
                                                       (message . ((body . ,str))))))))
                              (lambda (json)
                                nil))))

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
       (json-readtable-error
                    (message "ate bad json: %S" content)
                    (setq pos nl)))))
      (delete-region (point-min) (point)))))

(defun keybase--connect-to-server ()
  (let ((name " *keybase server*"))
    ;; Ensure that there is no buffer with this name already
    (when (get-buffer name)
      (error "keybase server buffer already exists"))
    (let* ((buf (get-buffer-create name))
           (pipe (make-pipe-process :name "keybase server error output"
                                    :buffer buf
                                    :filter (lambda (proc output)
                                              ;; Ignore error output
                                              nil))))
      (let ((proc (make-process :name "keybase server"
                                :buffer buf
                                :command '("keybase" "chat" "api-listen")
                                :coding 'utf-8
                                :filter 'keybase--filter-command
                                :stderr pipe)))
        (with-current-buffer buf
          (let ((json (keybase--request-api keybase--program '("status" "--json") nil)))
            (setq-local keybase--server-process proc)
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

(defun keybase-create-private-converstion (user)
  (interactive (let* ((v (get-char-property (point) 'keybase-user))
                      (default-name (or v (get-char-property (point) 'keybase-sender))))
                 (let ((name (read-string (if default-name
                                              (format "User (default %s): " default-name)
                                            "User: ")
                                          nil nil default-name nil)))
                   (list name))))
  (keybase-join-channel (keybase--private-conversation-channel-name user)))

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
                                            (keybase--render-private-list channels)))))
                                    :buffer (current-buffer))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode keybase-search-mode nil "Keybase search"
  "Mode for buffers containing keybase search results"
  (use-local-map keybase-search-map))

(defun keybase--format-search-results (json)
  (let ((hits (keybase--json-find json '(result hits))))
    (loop for entry across hits
          do (let ((msg (keybase--json-find entry '(hitMessage))))
               (keybase--with-json-bind ((message-id (valid messageID))
                                         (ctime (valid ctime))
                                         (sender-username (valid senderUsername))
                                         (message-type (valid messageBody messageType)))
                   msg
                 (when (eql message-type 1)
                   (let ((text (keybase--json-find msg '(valid messageBody text body))))
                     (keybase--insert-message-content message-id ctime sender-username text nil))))))))

(defun keybase--make-search-buffer ()
  (let ((buffer-name "*keybase search*"))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))
            buffer))
      ;; ELSE: Need to create the buffer
      (let ((buffer (generate-new-buffer buffer-name)))
        (with-current-buffer buffer
          (keybase-search-mode)
          (read-only-mode 1))
        buffer))))

(defun keybase-search-channel (query)
  (interactive "sQuery: ")
  (unless (eq major-mode 'keybase-channel-mode)
    (error "Not a channel buffer"))
  (let ((buffer (keybase--make-search-buffer))
        (channel-info keybase--channel-info))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (insert "Loading results\n"))
      (keybase--request-api-async keybase--program
                                  '("chat" "api")
                                  `((method . "searchregexp")
                                    (params . ((options . ((channel . ,(keybase--channel-info-as-json channel-info))
                                                           (query . ,query)
                                                           (is_regex . t))))))
                                  (lambda (json)
                                    (with-current-buffer buffer
                                      (let ((inhibit-read-only t))
                                        (delete-region (point-min) (point-max))
                                        (keybase--format-search-results json))))
                                  :buffer (current-buffer)))
    (pop-to-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar keybase-user-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode keybase-user-info-mode nil "Keybase user info"
  (use-local-map keybase-user-info-map))

(defun keybase--private-conversation-handler (user)
  (keybase-join-channel (keybase--private-conversation-channel-name user)))

(defun keybase--fill-in-user-lookup-results (json)
  (let ((userlist (keybase--json-find json '(them))))
    (unless (= (length userlist) 1)
      (error "Only able to fill in user info when the result has a single user"))
    (let ((user-result (aref userlist 0)))
      (keybase--with-json-bind ((user (basics username))
                                (full-name (profile full_name))
                                (bio (profile bio))
                                (location (profile location))
                                (primary-image (pictures primary url)))
          user-result
        (keybase--insert-image-url-async primary-image)
        (insert "\n")
        (insert (propertize "Name: " 'face 'bold) user "\n")
        (insert (propertize "Full name: " 'face 'bold) full-name "\n")
        (insert (propertize "Location: " 'face 'bold) location "\n")
        (insert (propertize "Bio: " 'face 'bold) bio "\n\n")
        (insert (keybase--make-clickable-button "Start private conversation" #'keybase--private-conversation-handler user))
        (insert "\n")))))

(defun keybase--start-load-user-info (user)
  (let ((buffer (current-buffer)))
    (keybase--request-api-async keybase--program
                                (list "apicall" "-a" (format "usernames=%s" user) "user/lookup")
                                nil
                                (lambda (json)
                                  (with-current-buffer buffer
                                    (let ((inhibit-read-only t))
                                      (keybase--fill-in-user-lookup-results json)))))))

(defun keybase-user-info (user)
  (interactive "sUsername: ")
  (let ((buffer (keybase--find-or-make-empty-buffer "*keybase user info*"
                                                    (lambda ()
                                                      (keybase-user-info-mode)
                                                      (read-only-mode 1)))))
    (with-current-buffer buffer
      (keybase--start-load-user-info user))
    (pop-to-buffer buffer)))

(provide 'keybase)
