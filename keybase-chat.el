;;; keybase-chat --- Keybase chat implementation in Emacs -*- lexical-binding: t -*-

(require 'msgpack)

(defvar *keybase--connection* nil)

(defun keybase--filter-network (proc output)
  (when (plusp (length output))
    (message "got output: %S" output)))

(defun keybase--find-keybase-socket ()
  (let ((xdg-dir (getenv "XDG_RUNTIME_DIR")))
    (unless xdg-dir
      (error "XDG_RUNTIME_DIR is not set"))
    (format "%s/keybase/keybased.sock" xdg-dir)))

(defun keybase--connect-to-remote (&optional addr)
  (let ((server-addr (or addr (keybase--find-keybase-socket))))
    (unless (file-exists-p server-addr)
      (error "Keybase socket not found, is keybase running?"))
    (condition-case err
        (let ((proc (make-network-process :name "keybase-remote"
                                          :buffer nil
                                          :family 'local
                                          :type nil
                                          :service server-addr
                                          :coding 'no-conversion)))
          (set-process-coding-system proc 'no-conversion 'no-conversion)
          (setq *keybase--connection* proc)
          (set-process-filter proc 'keybase--filter-network))
      ('file-error (error "err:%S type:%S" err (type-of err))))))

(defvar *keybase--request-id* 0)

(defun keybase--send-rpc (command &rest args)
  (let* ((content (msgpack-encode-value (vector 0 (incf *keybase--request-id*) command (apply #'vector args))))
         (length (length content)))
    (process-send-string *keybase--connection* (msgpack-encode-value length))
    (process-send-string *keybase--connection* content)))

(defun keybase--test-chat ()
  (keybase--connect-to-remote)
  (unless *keybase--connection*
    (error "no connection"))
  (keybase--send-rpc "chat.1.local.getGlobalAppNotificationSettingsLocal"))
