;;; zara.el --- Native Emacs client for Zara -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Thin native Emacs surface for Zara.  Requests go through the canonical
;; `zara' CLI, which remains responsible for ZARA/1 transport, authentication,
;; tool approval, memory, Prolog, plugins, and runtime policy.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup zara nil
  "Native Emacs client for Zara."
  :group 'applications
  :prefix "zara-")

(defcustom zara-program "zara"
  "Executable used for Zara requests."
  :type 'string
  :group 'zara)

(defcustom zara-connect-endpoint nil
  "Optional explicit ZARA/1 endpoint.

When nil, the Zara CLI resolves its normal configured/default daemon endpoint."
  :type '(choice (const :tag "Zara default" nil) string)
  :group 'zara)

(defcustom zara-chat-buffer-name "*Zara*"
  "Buffer used by `zara-chat'."
  :type 'string
  :group 'zara)

(defcustom zara-gptel-confirm-delegation t
  "When non-nil, gptel confirms before delegating a prompt to Zara."
  :type 'boolean
  :group 'zara)

(defvar-local zara-chat--busy nil)

(defun zara--arguments (prompt)
  "Return Zara CLI arguments for PROMPT."
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (user-error "Zara prompt must not be empty"))
  (append
   (when (and (stringp zara-connect-endpoint)
              (not (string-empty-p (string-trim zara-connect-endpoint))))
     (list "--connect" (string-trim zara-connect-endpoint)))
   (list prompt)))

(defun zara--program ()
  "Return the configured Zara executable or signal a user error."
  (or (executable-find zara-program)
      (user-error "Cannot find Zara executable: %s" zara-program)))

(defun zara--error-text (status output)
  "Return a concise error string for STATUS and OUTPUT."
  (let ((message (string-trim output)))
    (if (string-empty-p message)
        (format "Zara request failed with exit status %s" status)
      message)))

;;;###autoload
(defun zara-status ()
  "Return the current native Emacs Zara connection status."
  (interactive)
  (let* ((executable (executable-find zara-program))
         (status
          (list :program zara-program
                :executable executable
                :available (and executable t)
                :endpoint (or zara-connect-endpoint "configured/default"))))
    (when (called-interactively-p 'interactive)
      (message "Zara: %s | endpoint: %s"
               (if executable executable "not installed")
               (plist-get status :endpoint)))
    status))

;;;###autoload
(defun zara-ask (prompt)
  "Synchronously send PROMPT through Zara and return the response.

This is intended for gptel tools and Lisp callers.  Interactive chat uses
`zara-request' so Emacs stays responsive."
  (let ((program (zara--program))
        (arguments (zara--arguments prompt)))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil arguments)))
        (if (and (integerp status) (zerop status))
            (string-trim (buffer-string))
          (error "%s" (zara--error-text status (buffer-string))))))))

(defun zara-request (prompt callback)
  "Send PROMPT asynchronously and invoke CALLBACK with RESPONSE and ERROR.

Exactly one of RESPONSE or ERROR is non-nil."
  (let* ((program (zara--program))
         (arguments (zara--arguments prompt))
         (stdout (generate-new-buffer " *zara-stdout*"))
         (stderr (generate-new-buffer " *zara-stderr*")))
    (make-process
     :name "zara-emacs-request"
     :command (cons program arguments)
     :buffer stdout
     :stderr stderr
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let* ((status (process-exit-status process))
                (out (when (buffer-live-p stdout)
                       (with-current-buffer stdout
                         (string-trim (buffer-string)))))
                (err (when (buffer-live-p stderr)
                       (with-current-buffer stderr
                         (string-trim (buffer-string)))))
                (failure (string-trim
                          (string-join
                           (delq nil
                                 (list (unless (string-empty-p (or err "")) err)
                                       (unless (string-empty-p (or out "")) out)))
                           "\n"))))
           (when (buffer-live-p stdout)
             (kill-buffer stdout))
           (when (buffer-live-p stderr)
             (kill-buffer stderr))
           (if (zerop status)
               (funcall callback (or out "") nil)
             (funcall callback nil (zara--error-text status failure)))))))
    ))

(defun zara-chat--insert (speaker text)
  "Insert SPEAKER and TEXT into the current Zara chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "%s\n" speaker) 'face 'font-lock-keyword-face))
    (insert (string-trim (or text "")) "\n\n")
    (goto-char (point-max))))

(defvar zara-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "s") #'zara-chat-send)
    (define-key map (kbd "RET") #'zara-chat-send)
    (define-key map (kbd "g") #'zara-chat-refresh)
    map)
  "Keymap for `zara-chat-mode'.")

(define-derived-mode zara-chat-mode special-mode "Zara"
  "Major mode for the native Zara chat surface."
  (setq-local truncate-lines nil)
  (setq-local zara-chat--busy nil)
  (setq-local header-line-format
              '(:eval
                (format " Zara %s  ·  s/RET send  ·  g status"
                        (if zara-chat--busy "thinking…" "ready")))))

;;;###autoload
(defun zara-chat ()
  "Open the native Zara chat surface."
  (interactive)
  (let ((buffer (get-buffer-create zara-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'zara-chat-mode)
        (zara-chat-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Zara\n" 'face '(:height 1.5 :weight bold)))
          (insert "Canonical Zara runtime from Emacs.  Press s or RET to send.\n\n"))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun zara-chat-send (prompt)
  "Send PROMPT from the current Zara chat buffer."
  (interactive (list (read-string "Zara › ")))
  (when zara-chat--busy
    (user-error "Zara is already handling a request"))
  (setq zara-chat--busy t)
  (force-mode-line-update t)
  (zara-chat--insert "You" prompt)
  (let ((target (current-buffer)))
    (zara-request
     prompt
     (lambda (response error)
       (when (buffer-live-p target)
         (with-current-buffer target
           (setq zara-chat--busy nil)
           (zara-chat--insert
            (if error "Zara · error" "Zara")
            (or error response))
           (force-mode-line-update t)))))))

(defun zara-chat-refresh ()
  "Show current Zara executable and endpoint status."
  (interactive)
  (let ((status (zara-status)))
    (message "Zara %s · %s"
             (if (plist-get status :available) "available" "missing")
             (plist-get status :endpoint))))

;;;###autoload
(defun zara-gptel-register-tool ()
  "Register Zara as a bounded gptel delegation tool.

The tool sends one prompt through the canonical Zara client path.  Zara keeps
ownership of its own tool approvals and runtime policy."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "gptel is not available"))
  (when (fboundp 'gptel-get-tool)
    (ignore-errors
      (setf (gptel-get-tool "ZaraAsk") nil)))
  (gptel-make-tool
   :name "ZaraAsk"
   :function #'zara-ask
   :category "zara"
   :description
   "Delegate a self-contained request to the canonical Zara runtime and return its response."
   :args
   '((:name "prompt"
      :type string
      :description "Complete request to send to Zara"))
   :confirm zara-gptel-confirm-delegation)
  "ZaraAsk")

(provide 'zara)
;;; zara.el ends here
