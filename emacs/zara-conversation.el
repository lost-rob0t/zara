;;; zara-conversation.el --- Canonical Zara conversation control for Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (zara "0.2.0"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; Thin presentation control over Zara's canonical CLI/ZARA/1 conversation
;; owner.  Emacs keeps only ephemeral process/presentation state plus a stable
;; conversation id.  It owns no second transcript, provider, expert registry,
;; planner, permission system, or tool executor.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'zara)

(defgroup zara-conversation nil
  "Canonical Zara conversation control from Emacs."
  :group 'zara
  :prefix "zara-conversation-")

(defcustom zara-conversation-default-id "emacs-main"
  "Stable canonical Zara conversation id used by new Emacs chat buffers.

Keeping this stable lets Zara's durable conversation owner recover the same
conversation after Emacs restarts.  Emacs persists no parallel transcript."
  :type 'string
  :group 'zara-conversation)

(defconst zara-conversation--identifier-limit 128)
(defconst zara-conversation--context-limit 32)
(defconst zara-conversation--replay-version "ZARA-CONVERSATION-REPLAY/1")

(defvar-local zara-conversation-id nil)
(defvar-local zara-conversation-context-ids nil)
(defvar-local zara-conversation--generation 0)
(defvar-local zara-conversation--request-process nil)
(defvar-local zara-conversation--state 'idle)

(defun zara-conversation--validate-id (value label)
  "Return VALUE as a bounded opaque identifier for LABEL."
  (unless (stringp value)
    (user-error "%s must be a string" label))
  (let ((normalized (string-trim value)))
    (when (string-empty-p normalized)
      (user-error "%s must not be empty" label))
    (when (string-match-p "\0" normalized)
      (user-error "%s must not contain NUL" label))
    (when (> (length normalized) zara-conversation--identifier-limit)
      (user-error "%s exceeds %d characters"
                  label zara-conversation--identifier-limit))
    normalized))

(defun zara-conversation--current-id ()
  "Return the canonical conversation id for the current chat buffer."
  (setq-local
   zara-conversation-id
   (zara-conversation--validate-id
    (or zara-conversation-id zara-conversation-default-id)
    "conversation id")))

(defun zara-conversation--endpoint-arguments ()
  "Return optional canonical Zara endpoint arguments."
  (when (and (stringp zara-connect-endpoint)
             (not (string-empty-p (string-trim zara-connect-endpoint))))
    (list "--connect" (string-trim zara-connect-endpoint))))

(defun zara-conversation--context-arguments ()
  "Return bounded ephemeral context references for the native client."
  (unless (listp zara-conversation-context-ids)
    (user-error "context ids must be a list"))
  (when (> (length zara-conversation-context-ids)
           zara-conversation--context-limit)
    (user-error "context ids exceed maximum count %d"
                zara-conversation--context-limit))
  (cl-loop for value in zara-conversation-context-ids
           append
           (list "--context-id"
                 (zara-conversation--validate-id value "context id"))))

(defun zara-conversation--turn-arguments (conversation-id prompt)
  "Return native-client arguments for CONVERSATION-ID and PROMPT."
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (user-error "Zara prompt must not be empty"))
  (append
   (zara-conversation--endpoint-arguments)
   (list "--conversation-id"
         (zara-conversation--validate-id conversation-id "conversation id"))
   (zara-conversation--context-arguments)
   (list "--json-events" prompt)))

(defun zara-conversation--cancel-arguments (turn-id)
  "Return canonical CancelTurn CLI arguments for TURN-ID."
  (append
   (zara-conversation--endpoint-arguments)
   (list "--cancel-turn"
         (zara-conversation--validate-id turn-id "turn id"))))

(defun zara-conversation--replay-arguments (conversation-id)
  "Return local canonical replay arguments for CONVERSATION-ID."
  (list "--replay-conversation"
        (zara-conversation--validate-id conversation-id "conversation id")))

(defun zara-conversation--replay-required-string (object key label)
  "Return string KEY from replay OBJECT or signal an error using LABEL."
  (let ((value (gethash key object :missing)))
    (unless (stringp value)
      (error "replay %s must be a string" label))
    value))

(defun zara-conversation--validate-replay-message (message previous-sequence)
  "Validate replay MESSAGE after PREVIOUS-SEQUENCE and return its sequence."
  (unless (hash-table-p message)
    (error "replay message must be a JSON object"))
  (let ((sequence (gethash "sequence" message :missing))
        (role (gethash "role" message :missing))
        (content (gethash "content" message :missing))
        (status (gethash "status" message :missing))
        (turn-id (gethash "turn_id" message :missing))
        (error-text (gethash "error" message :missing))
        (tool-run-id (gethash "tool_run_id" message :missing)))
    (unless (and (integerp sequence) (> sequence 0))
      (error "replay message sequence must be a positive integer"))
    (when (and previous-sequence (<= sequence previous-sequence))
      (error "replay message sequence must be strictly increasing"))
    (unless (member role '("user" "assistant" "system" "tool"))
      (error "replay message role is invalid"))
    (unless (stringp content)
      (error "replay message content must be a string"))
    (unless (member status '("pending" "streaming" "complete" "error" "cancelled"))
      (error "replay message status is invalid"))
    (unless (or (eq turn-id :null) (stringp turn-id))
      (error "replay message turn_id must be null or a string"))
    (unless (stringp error-text)
      (error "replay message error must be a string"))
    (unless (or (eq tool-run-id :null) (stringp tool-run-id))
      (error "replay message tool_run_id must be null or a string"))
    sequence))

(defun zara-conversation--parse-replay (text expected-conversation-id)
  "Parse replay TEXT and require EXPECTED-CONVERSATION-ID."
  (let* ((payload
          (json-parse-string
           text
           :object-type 'hash-table
           :array-type 'list
           :null-object :null
           :false-object :false))
         (expected
          (zara-conversation--validate-id
           expected-conversation-id "conversation id")))
    (unless (hash-table-p payload)
      (error "replay payload must be a JSON object"))
    (unless (equal (gethash "version" payload) zara-conversation--replay-version)
      (error "unsupported conversation replay version"))
    (let ((conversation (gethash "conversation" payload :missing))
          (messages (gethash "messages" payload :missing)))
      (unless (hash-table-p conversation)
        (error "replay conversation must be a JSON object"))
      (unless (string=
               (zara-conversation--replay-required-string
                conversation "id" "conversation id")
               expected)
        (error "replay conversation id mismatch"))
      (zara-conversation--replay-required-string conversation "title" "title")
      (zara-conversation--replay-required-string
       conversation "created_at" "created_at")
      (zara-conversation--replay-required-string
       conversation "updated_at" "updated_at")
      (unless (listp messages)
        (error "replay messages must be a JSON array"))
      (let ((previous nil))
        (dolist (message messages)
          (setq previous
                (zara-conversation--validate-replay-message message previous))))
      payload)))

(defun zara-conversation--replay-speaker (role status)
  "Return presentation speaker for replay ROLE and STATUS."
  (let ((speaker
         (pcase role
           ("user" "You")
           ("assistant" "Zara")
           ("system" "Zara · system")
           ("tool" "Zara · tool")
           (_ (error "replay message role is invalid")))))
    (if (string= status "complete")
        speaker
      (format "%s · %s" speaker status))))

(defun zara-conversation--render-replay (payload)
  "Replace the current chat presentation with canonical replay PAYLOAD."
  (let* ((conversation (gethash "conversation" payload))
         (conversation-id (gethash "id" conversation))
         (title (gethash "title" conversation))
         (messages (gethash "messages" payload)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Zara\n" 'face '(:height 1.5 :weight bold)))
      (insert (format "Canonical conversation %s · %s\n\n"
                      conversation-id title)))
    (dolist (message messages)
      (let* ((role (gethash "role" message))
             (status (gethash "status" message))
             (content (gethash "content" message))
             (error-text (gethash "error" message))
             (rendered
              (if (string-empty-p error-text)
                  content
                (concat content "\n[" error-text "]"))))
        (zara-chat--insert
         (zara-conversation--replay-speaker role status)
         rendered)))
    (goto-char (point-max))))

(defun zara-conversation--current-generation-p (process)
  "Return non-nil when PROCESS still owns the target presentation generation."
  (let ((target (process-get process 'zara-target))
        (generation (process-get process 'zara-generation)))
    (and (buffer-live-p target)
         (with-current-buffer target
           (= generation zara-conversation--generation)))))

(defun zara-conversation--active-request-p (process)
  "Return non-nil when PROCESS is still the target buffer's active request."
  (let ((target (process-get process 'zara-target)))
    (and (buffer-live-p target)
         (with-current-buffer target
           (eq zara-conversation--request-process process)))))

(defun zara-conversation--finish-buffer (process state)
  "Finish PROCESS as STATE if it is still the active request.

Cancellation intentionally advances the presentation generation before the
runtime acknowledges CancelTurn.  Cleanup therefore uses request identity,
while assistant output remains generation-fenced."
  (let ((target (process-get process 'zara-target)))
    (when (zara-conversation--active-request-p process)
      (with-current-buffer target
        (setq zara-chat--busy nil
              zara-conversation--state state
              zara-conversation--request-process nil)
        (force-mode-line-update t)))))

(defun zara-conversation--deliver (process response error)
  "Deliver RESPONSE or ERROR once for PROCESS's current generation."
  (unless (process-get process 'zara-delivered)
    (process-put process 'zara-delivered t)
    (when (zara-conversation--current-generation-p process)
      (when-let ((callback (process-get process 'zara-callback)))
        (funcall callback response error)))))

(defun zara-conversation--protocol-error (process message)
  "Fail PROCESS closed after native protocol MESSAGE.

Once Zara has admitted a turn, a malformed or unexpected native-client event
must not merely hide its late output.  Fence presentation immediately and send
the runtime-minted turn id through canonical CancelTurn so a protocol failure
cannot leave the admitted turn running behind the Emacs surface.  If the
failure arrives before the turn receipt, keep parsing only long enough to learn
the id and cancel it; terminal process cleanup handles the no-receipt case."
  (unless (process-get process 'zara-protocol-error)
    (process-put process 'zara-protocol-error message)
    (zara-conversation--deliver process nil message)
    (process-put process 'zara-cancel-requested t)
    (process-put process 'zara-cancel-final-state 'error)
    (let ((target (process-get process 'zara-target)))
      (when (zara-conversation--active-request-p process)
        (with-current-buffer target
          (cl-incf zara-conversation--generation)
          (setq zara-conversation--state 'cancelling)
          (force-mode-line-update t))))
    (when-let ((turn-id (process-get process 'zara-turn-id)))
      (zara-conversation--start-cancel-required process turn-id)))
  nil)

(defun zara-conversation--cancel-error (process message)
  "Surface canonical cancellation MESSAGE for active PROCESS without stale text."
  (unless (process-get process 'zara-cancel-error)
    (process-put process 'zara-cancel-error message)
    (when (zara-conversation--active-request-p process)
      (unless (process-get process 'zara-delivered)
        (process-put process 'zara-delivered t)
        (when-let ((callback (process-get process 'zara-callback)))
          (funcall callback nil message)))
      (zara-conversation--finish-buffer process 'error))))

(defun zara-conversation--parse-event (line)
  "Parse one native-client NDJSON LINE or signal an error."
  (let ((event
         (json-parse-string
          line
          :object-type 'hash-table
          :array-type 'list
          :null-object nil
          :false-object :false)))
    (unless (hash-table-p event)
      (error "native-client event must be a JSON object"))
    event))

(defun zara-conversation--event-string (event key)
  "Return non-empty string KEY from EVENT or signal an error."
  (let ((value (gethash key event)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "native-client event %s must be a non-empty string" key))
    value))

(defun zara-conversation--cancel-stderr (process)
  "Return canonical cancel diagnostics for PROCESS."
  (let ((stderr (process-get process 'zara-stderr)))
    (if (buffer-live-p stderr)
        (with-current-buffer stderr (string-trim (buffer-string)))
      "")))

(defun zara-conversation--cancel-sentinel (process _event)
  "Finalize canonical CancelTurn PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let* ((request (process-get process 'zara-request-process))
           (status (process-exit-status process))
           (stderr (process-get process 'zara-stderr))
           (message (zara-conversation--cancel-stderr process))
           (final-state
            (or (process-get request 'zara-cancel-final-state) 'cancelled)))
      (when (buffer-live-p stderr)
        (kill-buffer stderr))
      (if (zerop status)
          (progn
            (process-put request 'zara-cancel-confirmed t)
            (zara-conversation--finish-buffer request final-state))
        (zara-conversation--cancel-error
         request
         (if (string-empty-p message)
             (format "CancelTurn failed with exit status %s" status)
           message))))))

(defun zara-conversation--start-cancel (request-process turn-id)
  "Submit canonical CancelTurn for REQUEST-PROCESS and TURN-ID."
  (unless (process-get request-process 'zara-cancel-process)
    (let* ((zara-connect-endpoint
            (process-get request-process 'zara-connect-endpoint))
           (stderr (generate-new-buffer " *zara-emacs-cancel-stderr*"))
           (cancel
            (make-process
             :name "zara-emacs-cancel"
             :command
             (cons (zara--program)
                   (zara-conversation--cancel-arguments turn-id))
             :buffer nil
             :stderr stderr
             :connection-type 'pipe
             :noquery t
             :sentinel #'zara-conversation--cancel-sentinel)))
      (process-put cancel 'zara-request-process request-process)
      (process-put cancel 'zara-stderr stderr)
      (process-put request-process 'zara-cancel-process cancel)))
  request-process)

(defun zara-conversation--start-cancel-required (request-process turn-id)
  "Start required CancelTurn for REQUEST-PROCESS and TURN-ID or fail closed."
  (condition-case error-data
      (zara-conversation--start-cancel request-process turn-id)
    (error
     (zara-conversation--cancel-error
      request-process
      (format "CancelTurn setup failed: %s" (error-message-string error-data)))
     nil)))

(defun zara-conversation--accept-turn (process event)
  "Handle one turn.accepted EVENT for PROCESS."
  (let* ((expected (process-get process 'zara-conversation-id))
         (conversation (zara-conversation--event-string event "conversation_id"))
         (turn-id (zara-conversation--event-string event "turn_id"))
         (existing (process-get process 'zara-turn-id)))
    (unless (string= conversation expected)
      (error "native-client event conversation_id mismatch"))
    (zara-conversation--validate-id turn-id "turn id")
    (when existing
      (if (string= existing turn-id)
          (error "duplicate turn.accepted")
        (error "native-client emitted conflicting turn ids")))
    (process-put process 'zara-turn-id turn-id)
    (when (process-get process 'zara-cancel-requested)
      (zara-conversation--start-cancel-required process turn-id))))

(defun zara-conversation--complete-turn (process event)
  "Handle one assistant.complete EVENT for PROCESS."
  (let* ((expected (process-get process 'zara-conversation-id))
         (conversation (zara-conversation--event-string event "conversation_id"))
         (turn-id (zara-conversation--event-string event "turn_id"))
         (accepted (process-get process 'zara-turn-id))
         (text (gethash "text" event)))
    (unless (string= conversation expected)
      (error "native-client event conversation_id mismatch"))
    (unless (and accepted (string= accepted turn-id))
      (error "assistant.complete does not match accepted turn"))
    (unless (stringp text)
      (error "assistant.complete text must be a string"))
    (when (process-get process 'zara-complete)
      (error "duplicate assistant.complete"))
    (process-put process 'zara-complete t)
    (unless (or (process-get process 'zara-cancel-requested)
                (process-get process 'zara-protocol-error))
      (zara-conversation--deliver process text nil)
      (zara-conversation--finish-buffer process 'complete))))

(defun zara-conversation--handle-line (process line)
  "Handle one complete native-client NDJSON LINE for PROCESS."
  (unless (string-empty-p (string-trim line))
    (condition-case error-data
        (let* ((event (zara-conversation--parse-event line))
               (type (zara-conversation--event-string event "type")))
          (pcase type
            ("turn.accepted" (zara-conversation--accept-turn process event))
            ("assistant.complete" (zara-conversation--complete-turn process event))
            (_ (error "unknown native-client event type: %s" type))))
      (error
       (zara-conversation--protocol-error
        process (error-message-string error-data))))))

(defun zara-conversation--process-filter (process chunk)
  "Consume native-client CHUNK from PROCESS as strict NDJSON."
  (let* ((pending (concat (or (process-get process 'zara-line-buffer) "") chunk))
         (lines (split-string pending "\n"))
         (tail (car (last lines))))
    (process-put process 'zara-line-buffer tail)
    (dolist (line (butlast lines))
      (zara-conversation--handle-line process line))))

(defun zara-conversation--request-stderr (process)
  "Return request stderr diagnostics for PROCESS."
  (let ((stderr (process-get process 'zara-stderr)))
    (if (buffer-live-p stderr)
        (with-current-buffer stderr (string-trim (buffer-string)))
      "")))

(defun zara-conversation--cancel-before-acceptance-error (process status)
  "Finish cancelled PROCESS that exited with STATUS before any turn receipt."
  (let ((message (zara-conversation--request-stderr process)))
    (zara-conversation--cancel-error
     process
     (if (string-empty-p message)
         (if (zerop status)
             "Zara request ended before cancellation received a turn id"
           (format "Zara request failed before cancellation received a turn id (exit %s)"
                   status))
       message))))

(defun zara-conversation--process-sentinel (process _event)
  "Finalize native-client PROCESS without accepting incomplete output."
  (when (memq (process-status process) '(exit signal))
    (let ((tail (or (process-get process 'zara-line-buffer) "")))
      (unless (string-empty-p (string-trim tail))
        (zara-conversation--protocol-error
         process "native-client ended with incomplete JSON event")))
    (let ((status (process-exit-status process))
          (stderr (process-get process 'zara-stderr)))
      (cond
       ((process-get process 'zara-protocol-error)
        (unless (or (process-get process 'zara-cancel-process)
                    (process-get process 'zara-cancel-confirmed))
          (if-let ((turn-id (process-get process 'zara-turn-id)))
              (zara-conversation--start-cancel-required process turn-id)
            (zara-conversation--finish-buffer process 'error))))
       ((process-get process 'zara-cancel-requested)
        (unless (or (process-get process 'zara-cancel-process)
                    (process-get process 'zara-cancel-confirmed))
          (zara-conversation--cancel-before-acceptance-error process status)))
       ((not (zerop status))
        (let ((message (zara-conversation--request-stderr process)))
          (zara-conversation--deliver
           process nil
           (if (string-empty-p message)
               (format "Zara request failed with exit status %s" status)
             message))
          (zara-conversation--finish-buffer process 'error)))
       ((not (process-get process 'zara-complete))
        (zara-conversation--deliver
         process nil "native-client ended without assistant.complete")
        (zara-conversation--finish-buffer process 'error)))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(defun zara-conversation-request (prompt callback)
  "Send PROMPT in the current canonical conversation and invoke CALLBACK.

CALLBACK receives RESPONSE and ERROR, matching `zara-request'.  The native CLI
must emit `turn.accepted' then matching `assistant.complete' NDJSON."
  (when zara-chat--busy
    (user-error "Zara is already handling a request"))
  (let* ((conversation-id (zara-conversation--current-id))
         (endpoint
          (and (stringp zara-connect-endpoint)
               (not (string-empty-p (string-trim zara-connect-endpoint)))
               (string-trim zara-connect-endpoint)))
         ;; Freeze endpoint authority for the whole turn.  A later settings
         ;; change must not redirect CancelTurn to another Zara server.
         (zara-connect-endpoint endpoint)
         (generation (cl-incf zara-conversation--generation))
         (stderr (generate-new-buffer " *zara-emacs-conversation-stderr*"))
         (process
          (make-process
           :name "zara-emacs-conversation"
           :command
           (cons (zara--program)
                 (zara-conversation--turn-arguments conversation-id prompt))
           :buffer nil
           :stderr stderr
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :filter #'zara-conversation--process-filter
           :sentinel #'zara-conversation--process-sentinel)))
    (process-put process 'zara-target (current-buffer))
    (process-put process 'zara-generation generation)
    (process-put process 'zara-conversation-id conversation-id)
    (process-put process 'zara-connect-endpoint endpoint)
    (process-put process 'zara-callback callback)
    (process-put process 'zara-stderr stderr)
    (process-put process 'zara-line-buffer "")
    (setq zara-chat--busy t
          zara-conversation--state 'running
          zara-conversation--request-process process)
    (force-mode-line-update t)
    process))

;;;###autoload
(defun zara-conversation-chat-send (prompt)
  "Send PROMPT from `zara-chat-mode' using canonical conversation control."
  (interactive (list (read-string "Zara › ")))
  (when zara-chat--busy
    (user-error "Zara is already handling a request"))
  (zara-chat--insert "You" prompt)
  (let ((target (current-buffer)))
    (zara-conversation-request
     prompt
     (lambda (response error)
       (when (buffer-live-p target)
         (with-current-buffer target
           (zara-chat--insert
            (if error "Zara · error" "Zara")
            (or error response))))))))

;;;###autoload
(defun zara-conversation-cancel ()
  "Cancel the active turn through Zara's canonical CancelTurn boundary."
  (interactive)
  (let ((process zara-conversation--request-process))
    (unless (and process (process-live-p process))
      (user-error "No active Zara turn"))
    (setq zara-conversation--state 'cancelling)
    ;; Fence assistant presentation immediately.  Keep parsing only enough to
    ;; obtain the runtime-minted turn id when cancellation beats acceptance.
    (cl-incf zara-conversation--generation)
    (process-put process 'zara-cancel-requested t)
    (when-let ((turn-id (process-get process 'zara-turn-id)))
      (zara-conversation--start-cancel-required process turn-id))
    (force-mode-line-update t)
    process))

;;;###autoload
(defun zara-conversation-switch (conversation-id)
  "Switch the Emacs surface to canonical CONVERSATION-ID while idle."
  (interactive
   (list (read-string "Zara conversation id: "
                      (or zara-conversation-id
                          zara-conversation-default-id))))
  (when zara-chat--busy
    (user-error "Cancel or finish the active Zara turn before switching"))
  (setq-local zara-conversation-id
              (zara-conversation--validate-id conversation-id "conversation id"))
  ;; Context refs are ephemeral presentation state.  Never carry refs from one
  ;; canonical conversation into another implicitly.
  (setq-local zara-conversation-context-ids nil)
  zara-conversation-id)

;;;###autoload
(defun zara-conversation-replay ()
  "Reload current durable conversation history into the Emacs presentation.

The CLI reads Zara's canonical conversation store directly.  Emacs receives a
strict JSON projection and persists no parallel transcript or conversation
state of its own."
  (interactive)
  (when zara-chat--busy
    (user-error "Cancel or finish the active Zara turn before replaying"))
  (let* ((target (current-buffer))
         (conversation-id (zara-conversation--current-id))
         (program (zara--program))
         (arguments (zara-conversation--replay-arguments conversation-id))
         payload)
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil arguments)))
        (unless (and (integerp status) (zerop status))
          (user-error "Zara conversation replay failed with exit status %s" status))
        (setq payload
              (zara-conversation--parse-replay
               (buffer-string) conversation-id))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (zara-conversation--render-replay payload)))
    payload))

;;;###autoload
(defun zara-conversation-status ()
  "Return and optionally display current canonical conversation state."
  (interactive)
  (let* ((process zara-conversation--request-process)
         (status
          (list :conversation-id (zara-conversation--current-id)
                :state zara-conversation--state
                :generation zara-conversation--generation
                :turn-id (and process (process-get process 'zara-turn-id))
                :cancel-requested
                (and process (process-get process 'zara-cancel-requested)))))
    (when (called-interactively-p 'interactive)
      (message "Zara conversation %s · %s%s"
               (plist-get status :conversation-id)
               (plist-get status :state)
               (if-let ((turn-id (plist-get status :turn-id)))
                   (format " · turn %s" turn-id)
                 "")))
    status))

(defvar zara-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap zara-chat-send] #'zara-conversation-chat-send)
    (define-key map (kbd "C-c C-k") #'zara-conversation-cancel)
    (define-key map (kbd "C-c C-s") #'zara-conversation-status)
    (define-key map (kbd "C-c C-c") #'zara-conversation-switch)
    (define-key map (kbd "C-c C-r") #'zara-conversation-replay)
    map)
  "Keymap for `zara-conversation-mode'.")

;;;###autoload
(define-minor-mode zara-conversation-mode
  "Use canonical streaming conversation identity and cancellation in Zara chat.

This mode is presentation-only and creates no model/provider fallback or second
conversation owner."
  :lighter " ZaraConv"
  :keymap zara-conversation-mode-map
  (when zara-conversation-mode
    (zara-conversation--current-id)))

(defun zara-conversation--enable-chat-mode ()
  "Enable canonical conversation control in a Zara chat buffer."
  (zara-conversation-mode 1))

(add-hook 'zara-chat-mode-hook #'zara-conversation--enable-chat-mode)

;;;###autoload
(defun zara-conversation-chat ()
  "Open `zara-chat' with canonical conversation control enabled."
  (interactive)
  (zara-chat)
  (zara-conversation-mode 1))

(provide 'zara-conversation)
;;; zara-conversation.el ends here
