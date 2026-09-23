;;; zara.el --- Native Emacs client and semantic bridge for Zara -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; Native Emacs surface for Zara.
;;
;; Conversation requests still go through the canonical `zara' CLI/runtime.
;; Editor control goes through the versioned ZARA-EMACS/1 semantic bridge
;; implemented here.  The bridge accepts JSON requests for a closed operation
;; registry; request text is never evaluated as Elisp.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup zara nil
  "Native Emacs client for Zara."
  :group 'applications
  :prefix "zara-")

(defcustom zara-program "zara"
  "Executable used for Zara conversation requests."
  :type 'string
  :group 'zara)

(defcustom zara-connect-endpoint nil
  "Optional explicit ZARA/1 endpoint.

When nil, the Zara CLI resolves its configured/default daemon endpoint."
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

(defcustom zara-bridge-max-read-chars 32768
  "Maximum number of buffer characters returned by one bridge read."
  :type 'integer
  :group 'zara)

(defcustom zara-bridge-max-edit-chars 32768
  "Maximum replacement size accepted by one bridge edit proposal."
  :type 'integer
  :group 'zara)

(defconst zara-bridge-version "ZARA-EMACS/1"
  "Wire-level semantic bridge version.")

(defvar-local zara-chat--busy nil)
(defvar zara-bridge--buffer-ids (make-hash-table :test #'eq))
(defvar zara-bridge--buffers (make-hash-table :test #'equal))
(defvar zara-bridge--window-ids (make-hash-table :test #'eq))
(defvar zara-bridge--windows (make-hash-table :test #'equal))
(defvar zara-bridge--edit-proposals (make-hash-table :test #'equal))
(defvar zara-bridge--handlers (make-hash-table :test #'equal))
(defvar zara-bridge--command-adapters (make-hash-table :test #'equal))
(defvar zara-bridge--buffer-counter 0)
(defvar zara-bridge--window-counter 0)
(defvar zara-bridge--edit-counter 0)
(defvar zara-bridge--session-id
  (substring
   (secure-hash
    'sha256
    (format "%s:%s:%s:%s"
            (emacs-pid)
            (float-time)
            (system-name)
            (random most-positive-fixnum)))
   0 24))

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
  (let ((message (string-trim (or output ""))))
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
                :endpoint (or zara-connect-endpoint "configured/default")
                :bridge zara-bridge-version
                :session zara-bridge--session-id
                :native-mode zara-native-mode)))
    (when (called-interactively-p 'interactive)
      (message "Zara: %s | endpoint: %s | bridge: %s"
               (if executable executable "not installed")
               (plist-get status :endpoint)
               zara-bridge-version))
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
                (failure
                 (string-trim
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
             (funcall callback nil (zara--error-text status failure)))))))))

(defun zara-chat--insert (speaker text)
  "Insert SPEAKER and TEXT into the current Zara chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "%s\n" speaker)
                        'face 'font-lock-keyword-face))
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
          (insert
           "Canonical Zara runtime from Emacs.  Press s or RET to send.\n\n"))))
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
  "Show current Zara executable, endpoint, and bridge status."
  (interactive)
  (let ((status (zara-status)))
    (message "Zara %s · %s · %s"
             (if (plist-get status :available) "available" "missing")
             (plist-get status :endpoint)
             (plist-get status :bridge))))

;;; Semantic bridge

(defun zara-bridge--json-bool (value)
  "Return VALUE represented as a JSON boolean."
  (if value t :false))

(defun zara-bridge--json-array (values)
  "Return VALUES in the vector representation required for a JSON array."
  (vconcat values))

(defun zara-bridge--args (request)
  "Return the args hash table from REQUEST."
  (let ((args (gethash "args" request)))
    (cond
     ((null args) (make-hash-table :test #'equal))
     ((hash-table-p args) args)
     (t (error "args must be a JSON object")))))

(defun zara-bridge--string (args key &optional required maximum)
  "Read string KEY from ARGS and enforce REQUIRED and MAXIMUM."
  (let ((value (gethash key args)))
    (when (and required
               (or (not (stringp value)) (string-empty-p value)))
      (error "%s is required" key))
    (when (and value (not (stringp value)))
      (error "%s must be a string" key))
    (when (and value maximum (> (length value) maximum))
      (error "%s exceeds maximum length %d" key maximum))
    (when (and value (string-match-p "\0" value))
      (error "%s contains NUL" key))
    value))

(defun zara-bridge--integer (args key &optional required minimum maximum)
  "Read integer KEY from ARGS with bounds."
  (let ((value (gethash key args)))
    (when (and required (not (integerp value)))
      (error "%s is required" key))
    (when (and value (not (integerp value)))
      (error "%s must be an integer" key))
    (when (and value minimum (< value minimum))
      (error "%s must be >= %d" key minimum))
    (when (and value maximum (> value maximum))
      (error "%s must be <= %d" key maximum))
    value))

(defun zara-bridge--buffer-id (buffer)
  "Return stable opaque id for BUFFER in this Emacs session."
  (or (gethash buffer zara-bridge--buffer-ids)
      (let ((id (format "b-%d" (cl-incf zara-bridge--buffer-counter))))
        (puthash buffer id zara-bridge--buffer-ids)
        (puthash id buffer zara-bridge--buffers)
        id)))

(defun zara-bridge--buffer (id)
  "Resolve opaque buffer ID and reject stale buffers."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "buffer_id is required"))
  (let ((buffer (gethash id zara-bridge--buffers)))
    (unless (buffer-live-p buffer)
      (remhash id zara-bridge--buffers)
      (error "unknown or stale buffer_id"))
    buffer))

(defun zara-bridge--window-id (window)
  "Return stable opaque id for WINDOW in this Emacs session."
  (or (gethash window zara-bridge--window-ids)
      (let ((id (format "w-%d" (cl-incf zara-bridge--window-counter))))
        (puthash window id zara-bridge--window-ids)
        (puthash id window zara-bridge--windows)
        id)))

(defun zara-bridge--window (id)
  "Resolve opaque window ID and reject stale windows."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "window_id is required"))
  (let ((window (gethash id zara-bridge--windows)))
    (unless (window-live-p window)
      (remhash id zara-bridge--windows)
      (error "unknown or stale window_id"))
    window))

(defun zara-bridge--project-root ()
  "Return the current project root, or nil."
  (when (require 'project nil t)
    (when-let ((project (project-current nil)))
      (expand-file-name (project-root project)))))

(defun zara-bridge--active-minor-modes ()
  "Return bounded active minor mode names as a JSON array."
  (let (modes)
    (dolist (mode minor-mode-list)
      (when (and (boundp mode) (symbol-value mode))
        (push (symbol-name mode) modes)))
    (zara-bridge--json-array (seq-take (nreverse modes) 64))))

(defun zara-bridge--buffer-context (buffer)
  "Return structured context for BUFFER."
  (with-current-buffer buffer
    (let* ((mark-pos (and (mark t) (marker-position (mark-marker))))
           (region-active (and transient-mark-mode mark-active mark-pos))
           (file (buffer-file-name)))
      `((buffer_id . ,(zara-bridge--buffer-id buffer))
        (name . ,(buffer-name))
        (file . ,file)
        (major_mode . ,(symbol-name major-mode))
        (minor_modes . ,(zara-bridge--active-minor-modes))
        (point . ,(point))
        (mark . ,mark-pos)
        (region_active . ,(zara-bridge--json-bool region-active))
        (region_start . ,(when region-active (region-beginning)))
        (region_end . ,(when region-active (region-end)))
        (point_min . ,(point-min))
        (point_max . ,(point-max))
        (narrowed . ,(zara-bridge--json-bool (buffer-narrowed-p)))
        (read_only . ,(zara-bridge--json-bool buffer-read-only))
        (modified . ,(zara-bridge--json-bool (buffer-modified-p)))
        (modified_tick . ,(buffer-chars-modified-tick))
        (default_directory . ,default-directory)
        (project_root . ,(zara-bridge--project-root))))))

(defun zara-bridge--target-buffer (args)
  "Return buffer selected by ARGS or the current buffer."
  (if-let ((id (zara-bridge--string args "buffer_id")))
      (zara-bridge--buffer id)
    (current-buffer)))

(defun zara-bridge--op-session-describe (_args)
  "Implement session.describe."
  `((bridge . ,zara-bridge-version)
    (session_id . ,zara-bridge--session-id)
    (emacs_version . ,emacs-version)
    (server_name . ,(and (boundp 'server-name) server-name))
    (native_mode . ,(zara-bridge--json-bool zara-native-mode))
    (capabilities
     . ["session.describe"
        "buffer.list" "buffer.context" "buffer.read" "buffer.open" "buffer.switch"
        "window.list" "window.select" "window.split" "window.delete"
        "command.list" "command.describe" "command.where_is" "command.key_lookup"
        "command.invoke"
        "edit.preview" "edit.apply" "edit.cancel" "edit.status"
        "buffer.save"
        "ui.scratch" "ui.buffer_by_name" "ui.zara_chat" "ui.ai_dashboard"
        "org_roam.open_daily" "magit.open_project"])))

(defun zara-bridge--op-buffer-list (args)
  "Implement buffer.list."
  (let ((limit (or (zara-bridge--integer args "limit" nil 1 200) 100))
        rows)
    (dolist (buffer (buffer-list))
      (when (< (length rows) limit)
        (with-current-buffer buffer
          (push `((buffer_id . ,(zara-bridge--buffer-id buffer))
                  (name . ,(buffer-name))
                  (file . ,(buffer-file-name))
                  (major_mode . ,(symbol-name major-mode))
                  (modified . ,(zara-bridge--json-bool (buffer-modified-p)))
                  (read_only . ,(zara-bridge--json-bool buffer-read-only))
                  (modified_tick . ,(buffer-chars-modified-tick)))
                rows))))
    `((buffers . ,(zara-bridge--json-array (nreverse rows)))
      (count . ,(length rows)))))

(defun zara-bridge--op-buffer-context (args)
  "Implement buffer.context."
  (zara-bridge--buffer-context (zara-bridge--target-buffer args)))

(defun zara-bridge--op-buffer-read (args)
  "Implement buffer.read."
  (let ((buffer (zara-bridge--target-buffer args)))
    (with-current-buffer buffer
      (let* ((start (or (zara-bridge--integer args "start") (point-min)))
             (end (or (zara-bridge--integer args "end") (point-max)))
             (available-min (point-min))
             (available-max (point-max)))
        (unless (and (<= available-min start)
                     (<= start end)
                     (<= end available-max))
          (error "requested range is outside the accessible buffer"))
        (when (> (- end start) zara-bridge-max-read-chars)
          (error "requested range exceeds zara-bridge-max-read-chars"))
        `((buffer_id . ,(zara-bridge--buffer-id buffer))
          (start . ,start)
          (end . ,end)
          (modified_tick . ,(buffer-chars-modified-tick))
          (text . ,(buffer-substring-no-properties start end)))))))

(defun zara-bridge--absolute-path (args key)
  "Read absolute file path KEY from ARGS."
  (let ((path (zara-bridge--string args key t 4096)))
    (unless (file-name-absolute-p path)
      (error "%s must be an absolute path" key))
    (expand-file-name path)))

(defun zara-bridge--op-buffer-open (args)
  "Implement buffer.open."
  (let* ((path (zara-bridge--absolute-path args "path"))
         (buffer (find-file-noselect path)))
    (switch-to-buffer buffer)
    (zara-bridge--buffer-context buffer)))

(defun zara-bridge--op-buffer-switch (args)
  "Implement buffer.switch."
  (let ((buffer
         (zara-bridge--buffer
          (zara-bridge--string args "buffer_id" t 128))))
    (switch-to-buffer buffer)
    (zara-bridge--buffer-context buffer)))

(defun zara-bridge--op-ui-buffer-by-name (args)
  "Implement compatibility ui.buffer_by_name."
  (let* ((name (zara-bridge--string args "name" t 256))
         (buffer (get-buffer-create name)))
    (switch-to-buffer buffer)
    (zara-bridge--buffer-context buffer)))

(defun zara-bridge--window-row (window)
  "Return structured metadata for WINDOW."
  (let ((edges (window-edges window)))
    `((window_id . ,(zara-bridge--window-id window))
      (buffer_id . ,(zara-bridge--buffer-id (window-buffer window)))
      (selected . ,(zara-bridge--json-bool (eq window (selected-window))))
      (start . ,(window-start window))
      (point . ,(window-point window))
      (edges . ,(vconcat edges)))))

(defun zara-bridge--op-window-list (_args)
  "Implement window.list for the selected frame."
  (let ((windows (window-list (selected-frame) nil)))
    `((windows . ,(zara-bridge--json-array
                   (mapcar #'zara-bridge--window-row windows)))
      (count . ,(length windows)))))

(defun zara-bridge--op-window-select (args)
  "Implement window.select."
  (let ((window
         (zara-bridge--window
          (zara-bridge--string args "window_id" t 128))))
    (select-window window)
    (zara-bridge--window-row window)))

(defun zara-bridge--op-window-split (args)
  "Implement window.split."
  (let* ((window
          (zara-bridge--window
           (zara-bridge--string args "window_id" t 128)))
         (side (or (zara-bridge--string args "side") "below"))
         (size (zara-bridge--integer args "size" nil 1))
         (side-symbol
          (pcase side
            ("below" 'below)
            ("right" 'right)
            (_ (error "side must be below or right"))))
         (created (split-window window size side-symbol)))
    (zara-bridge--window-row created)))

(defun zara-bridge--op-window-delete (args)
  "Implement window.delete."
  (let ((window
         (zara-bridge--window
          (zara-bridge--string args "window_id" t 128))))
    (delete-window window)
    '((deleted . t))))

(defun zara-bridge--op-command-list (args)
  "Implement bounded command.list."
  (let* ((query (downcase (or (zara-bridge--string args "query" nil 256) "")))
         (limit (or (zara-bridge--integer args "limit" nil 1 200) 100))
         names)
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (< (length names) limit)
                  (string-match-p (regexp-quote query)
                                  (downcase (symbol-name symbol))))
         (push (symbol-name symbol) names))))
    `((commands . ,(zara-bridge--json-array (sort names #'string<)))
      (count . ,(length names)))))

(defun zara-bridge--command-symbol (args)
  "Resolve a command named in ARGS without interning new symbols."
  (let* ((name (zara-bridge--string args "command" t 256))
         (symbol (intern-soft name)))
    (unless (and symbol (commandp symbol))
      (error "unknown Emacs command"))
    symbol))

(defun zara-bridge--op-command-describe (args)
  "Implement command.describe."
  (let* ((command (zara-bridge--command-symbol args))
         (doc (documentation command t))
         (keys (where-is-internal command nil nil nil)))
    `((command . ,(symbol-name command))
      (documentation . ,(when doc (substring doc 0 (min 8192 (length doc)))))
      (keys . ,(zara-bridge--json-array (mapcar #'key-description keys))))))

(defun zara-bridge--op-command-where-is (args)
  "Implement command.where_is."
  (let* ((command (zara-bridge--command-symbol args))
         (keys (where-is-internal command nil nil nil)))
    `((command . ,(symbol-name command))
      (keys . ,(zara-bridge--json-array (mapcar #'key-description keys))))))

(defun zara-bridge--op-command-key-lookup (args)
  "Implement command.key_lookup."
  (let* ((key (zara-bridge--string args "key" t 256))
         (binding (key-binding (kbd key))))
    `((key . ,key)
      (command . ,(when (symbolp binding) (symbol-name binding)))
      (bound . ,(zara-bridge--json-bool binding)))))

(defun zara-register-command-adapter (name function)
  "Register trusted command adapter NAME backed by FUNCTION.

NAME is the only value exposed over command.invoke.  FUNCTION receives the
decoded args hash table and returns a JSON-serializable value."
  (unless (and (stringp name)
               (string-match-p "\\`[a-z0-9][a-z0-9._-]*\\'" name))
    (error "invalid Zara command adapter name"))
  (unless (functionp function)
    (error "command adapter must be callable"))
  (puthash name function zara-bridge--command-adapters)
  name)

(defun zara-bridge--op-command-invoke (args)
  "Implement closed command.invoke."
  (let* ((adapter (zara-bridge--string args "adapter" t 128))
         (function (gethash adapter zara-bridge--command-adapters)))
    (unless function
      (error "unknown or unavailable command adapter"))
    (funcall function args)))

(defun zara-bridge--validate-edit-range (buffer start end expected-tick)
  "Validate edit preconditions for BUFFER START END EXPECTED-TICK."
  (with-current-buffer buffer
    (when buffer-read-only
      (error "buffer is read-only"))
    (unless (= expected-tick (buffer-chars-modified-tick))
      (error "stale buffer revision"))
    (unless (and (<= (point-min) start)
                 (<= start end)
                 (<= end (point-max)))
      (error "edit range is outside the accessible buffer"))))

(defun zara-bridge--op-edit-preview (args)
  "Implement edit.preview."
  (let* ((buffer
          (zara-bridge--buffer
           (zara-bridge--string args "buffer_id" t 128)))
         (start (zara-bridge--integer args "start" t 1))
         (end (zara-bridge--integer args "end" t 1))
         (expected-tick
          (zara-bridge--integer args "expected_tick" t 0))
         (replacement
          (or (zara-bridge--string
               args "replacement" nil zara-bridge-max-edit-chars)
              "")))
    (zara-bridge--validate-edit-range buffer start end expected-tick)
    (with-current-buffer buffer
      (let* ((old (buffer-substring-no-properties start end))
             (id (format "e-%d" (cl-incf zara-bridge--edit-counter)))
             (proposal
              (list :id id
                    :buffer buffer
                    :buffer-id (zara-bridge--buffer-id buffer)
                    :start start
                    :end end
                    :tick expected-tick
                    :replacement replacement
                    :before old
                    :state "previewed")))
        (puthash id proposal zara-bridge--edit-proposals)
        `((edit_id . ,id)
          (buffer_id . ,(plist-get proposal :buffer-id))
          (start . ,start)
          (end . ,end)
          (expected_tick . ,expected-tick)
          (before . ,old)
          (after . ,replacement)
          (state . "previewed"))))))

(defun zara-bridge--edit (args)
  "Resolve edit proposal from ARGS."
  (let* ((id (zara-bridge--string args "edit_id" t 128))
         (proposal (gethash id zara-bridge--edit-proposals)))
    (unless proposal
      (error "unknown edit_id"))
    proposal))

(defun zara-bridge--op-edit-apply (args)
  "Implement edit.apply."
  (let* ((proposal (zara-bridge--edit args))
         (state (plist-get proposal :state)))
    (unless (string= state "previewed")
      (error "edit is not in previewed state"))
    (let ((buffer (plist-get proposal :buffer))
          (start (plist-get proposal :start))
          (end (plist-get proposal :end))
          (tick (plist-get proposal :tick))
          (replacement (plist-get proposal :replacement)))
      (unless (buffer-live-p buffer)
        (error "edit target buffer is stale"))
      (zara-bridge--validate-edit-range buffer start end tick)
      (with-current-buffer buffer
        (atomic-change-group
          (goto-char start)
          (delete-region start end)
          (insert replacement))
        (setf (plist-get proposal :state) "applied")
        (puthash (plist-get proposal :id)
                 proposal zara-bridge--edit-proposals)
        `((edit_id . ,(plist-get proposal :id))
          (state . "applied")
          (buffer_id . ,(plist-get proposal :buffer-id))
          (modified_tick . ,(buffer-chars-modified-tick)))))))

(defun zara-bridge--op-edit-cancel (args)
  "Implement edit.cancel."
  (let ((proposal (zara-bridge--edit args)))
    (when (string= (plist-get proposal :state) "applied")
      (error "applied edits cannot be cancelled"))
    (setf (plist-get proposal :state) "cancelled")
    (puthash (plist-get proposal :id)
             proposal zara-bridge--edit-proposals)
    `((edit_id . ,(plist-get proposal :id))
      (state . "cancelled"))))

(defun zara-bridge--op-edit-status (args)
  "Implement edit.status."
  (let ((proposal (zara-bridge--edit args)))
    `((edit_id . ,(plist-get proposal :id))
      (state . ,(plist-get proposal :state))
      (buffer_id . ,(plist-get proposal :buffer-id))
      (expected_tick . ,(plist-get proposal :tick)))))

(defun zara-bridge--op-buffer-save (args)
  "Implement buffer.save."
  (let ((buffer (zara-bridge--target-buffer args)))
    (with-current-buffer buffer
      (unless (buffer-file-name)
        (error "buffer is not visiting a file"))
      (save-buffer)
      `((buffer_id . ,(zara-bridge--buffer-id buffer))
        (file . ,(buffer-file-name))
        (modified . ,(zara-bridge--json-bool (buffer-modified-p)))
        (modified_tick . ,(buffer-chars-modified-tick))))))

(defun zara-bridge--op-ui-scratch (_args)
  "Implement ui.scratch."
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (zara-bridge--buffer-context (current-buffer)))

(defun zara-bridge--op-ui-zara-chat (_args)
  "Implement ui.zara_chat."
  (zara-chat)
  '((opened . t)))

(defun zara-bridge--op-ui-ai-dashboard (_args)
  "Implement ui.ai_dashboard."
  (unless (require 'ai-dashboard nil t)
    (error "ai-dashboard unavailable"))
  (unless (fboundp 'ai/dashboard)
    (error "ai/dashboard unavailable"))
  (call-interactively #'ai/dashboard)
  '((opened . t)))

(defun zara-bridge--op-org-roam-open-daily (args)
  "Implement org_roam.open_daily."
  (unless (require 'org-roam-dailies nil t)
    (error "org-roam-dailies unavailable"))
  (let ((day (or (zara-bridge--string args "date" nil 32)
                 (format-time-string "%Y-%m-%d"))))
    (unless (string-match-p
             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" day)
      (error "date must be ISO YYYY-MM-DD"))
    (org-roam-dailies--capture
     (org-read-date nil t day) t nil)
    `((date . ,day)
      (buffer . ,(buffer-name))
      (file . ,(buffer-file-name))
      (post_open . ((request . "dictation") (started . :false))))))

(defun zara-bridge--op-magit-open-project (args)
  "Implement magit.open_project."
  (unless (require 'magit nil t)
    (error "Magit unavailable"))
  (let ((path (zara-bridge--absolute-path args "path")))
    (unless (file-directory-p path)
      (error "project path is not a directory"))
    (magit-status path)
    `((path . ,path)
      (opened . t))))

(defun zara-bridge-register-handler (operation function)
  "Register trusted bridge OPERATION backed by FUNCTION."
  (unless (and (stringp operation)
               (string-match-p "\\`[a-z0-9][a-z0-9._-]*\\'" operation))
    (error "invalid Zara bridge operation"))
  (unless (functionp function)
    (error "bridge handler must be callable"))
  (puthash operation function zara-bridge--handlers)
  operation)

(defun zara-bridge--register-builtins ()
  "Register the built-in ZARA-EMACS/1 operation set."
  (clrhash zara-bridge--handlers)
  (dolist
      (entry
       '(("session.describe" . zara-bridge--op-session-describe)
         ("buffer.list" . zara-bridge--op-buffer-list)
         ("buffer.context" . zara-bridge--op-buffer-context)
         ("buffer.read" . zara-bridge--op-buffer-read)
         ("buffer.open" . zara-bridge--op-buffer-open)
         ("buffer.switch" . zara-bridge--op-buffer-switch)
         ("window.list" . zara-bridge--op-window-list)
         ("window.select" . zara-bridge--op-window-select)
         ("window.split" . zara-bridge--op-window-split)
         ("window.delete" . zara-bridge--op-window-delete)
         ("command.list" . zara-bridge--op-command-list)
         ("command.describe" . zara-bridge--op-command-describe)
         ("command.where_is" . zara-bridge--op-command-where-is)
         ("command.key_lookup" . zara-bridge--op-command-key-lookup)
         ("command.invoke" . zara-bridge--op-command-invoke)
         ("edit.preview" . zara-bridge--op-edit-preview)
         ("edit.apply" . zara-bridge--op-edit-apply)
         ("edit.cancel" . zara-bridge--op-edit-cancel)
         ("edit.status" . zara-bridge--op-edit-status)
         ("buffer.save" . zara-bridge--op-buffer-save)
         ("ui.scratch" . zara-bridge--op-ui-scratch)
         ("ui.buffer_by_name" . zara-bridge--op-ui-buffer-by-name)
         ("ui.zara_chat" . zara-bridge--op-ui-zara-chat)
         ("ui.ai_dashboard" . zara-bridge--op-ui-ai-dashboard)
         ("org_roam.open_daily" . zara-bridge--op-org-roam-open-daily)
         ("magit.open_project" . zara-bridge--op-magit-open-project)))
    (zara-bridge-register-handler (car entry) (cdr entry))))

(defun zara-bridge--response (operation ok value)
  "Serialize bridge response for OPERATION with OK and VALUE."
  (json-serialize
   `((bridge . ,zara-bridge-version)
     (session_id . ,zara-bridge--session-id)
     (operation . ,operation)
     (ok . ,(zara-bridge--json-bool ok))
     (,(if ok 'result 'error) . ,value))
   :null-object nil
   :false-object :false))

;;;###autoload
(defun zara-bridge-call (encoded-request)
  "Execute one base64-encoded ZARA-EMACS/1 JSON request.

The request chooses only a registered semantic operation.  Request data is
decoded as JSON and never evaluated as Lisp."
  (let ((operation "unknown"))
    (condition-case error-data
        (let* ((json-text
                (decode-coding-string
                 (base64-decode-string encoded-request)
                 'utf-8-unix))
               (request
                (json-parse-string
                 json-text
                 :object-type 'hash-table
                 :array-type 'list
                 :null-object nil
                 :false-object :false))
               (bridge (gethash "bridge" request))
               (candidate (gethash "operation" request))
               (args (zara-bridge--args request)))
          (unless (string= bridge zara-bridge-version)
            (error "unsupported bridge version"))
          (unless (and (stringp candidate)
                       (not (string-empty-p candidate)))
            (error "operation is required"))
          (setq operation candidate)
          (let ((handler (gethash operation zara-bridge--handlers)))
            (unless handler
              (error "unknown bridge operation"))
            (zara-bridge--response
             operation t (funcall handler args))))
      (error
       (zara-bridge--response
        operation nil
        `((code . "operation_failed")
          (message . ,(error-message-string error-data))))))))

(defun zara-native-context-json ()
  "Return current Emacs context as JSON for Lisp/gptel callers."
  (json-serialize
   (zara-bridge--buffer-context (current-buffer))
   :null-object nil
   :false-object :false))

;;;###autoload
(defun zara-native-describe-context ()
  "Display the current structured Zara/Emacs buffer context."
  (interactive)
  (let ((context (zara-bridge--buffer-context (current-buffer))))
    (with-help-window "*Zara Context*"
      (princ (pp-to-string context)))))

;;;###autoload
(defun zara-ask-dwim (prompt)
  "Send active region text to Zara, or read PROMPT when no region is active."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Zara › "))))
  (let ((buffer (get-buffer-create zara-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'zara-chat-mode)
        (zara-chat-mode))
      (zara-chat-send prompt))
    (pop-to-buffer buffer)))

;;;###autoload
(defun zara-gptel-register-tools ()
  "Register Zara conversation and Emacs-context tools with gptel."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "gptel is not available"))
  (dolist (name '("ZaraAsk" "ZaraEmacsContext"))
    (when (fboundp 'gptel-get-tool)
      (ignore-errors
        (setf (gptel-get-tool name) nil))))
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
  (gptel-make-tool
   :name "ZaraEmacsContext"
   :function #'zara-native-context-json
   :category "zara"
   :description
   "Return bounded structured context for the currently selected Emacs buffer."
   :args nil
   :confirm nil)
  '("ZaraAsk" "ZaraEmacsContext"))

;;;###autoload
(defun zara-gptel-register-tool ()
  "Compatibility wrapper for `zara-gptel-register-tools'."
  (interactive)
  (zara-gptel-register-tools))

(defvar zara-native-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z z") #'zara-chat)
    (define-key map (kbd "C-c z a") #'zara-ask-dwim)
    (define-key map (kbd "C-c z s") #'zara-status)
    (define-key map (kbd "C-c z c") #'zara-native-describe-context)
    map)
  "Keymap for `zara-native-mode'.")

;;;###autoload
(define-minor-mode zara-native-mode
  "Global native Zara integration for Emacs.

This mode exposes the semantic bridge and convenient native commands.  It does
not start a microphone, daemon, LLM, or second editor runtime."
  :global t
  :lighter " Zara"
  :keymap zara-native-mode-map)

(zara-bridge--register-builtins)

(provide 'zara)
;;; zara.el ends here
