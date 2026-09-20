;;; zara-conversation.el --- Canonical Zara conversation control for Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (zara "0.2.0"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; Stream native Zara CLI events into the existing `zara-chat-mode' surface.
;; Emacs keeps only ephemeral presentation state plus a stable canonical
;; conversation identifier.  Zara remains the owner of conversation history,
;; runtime policy, expert dispatch, tools/effects, cancellation, and durable
;; replay.  This module never talks to a provider and never owns a second
;; conversation store.

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

Keeping this value stable lets Zara's own durable conversation owner recover the
same conversation after an Emacs process restart.  Emacs does not persist a
parallel transcript."
  :type 'string
  :group 'zara-conversation)

(defconst zara-conversation--identifier-limit 128
  "Maximum identifier size accepted by the Zara native-client contract.")

(defvar-local zara-conversation-id nil)
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

(defun zara-conversation--turn-arguments (conversation-id prompt)
  "Return native-client arguments for CONVERSATION-ID and PROMPT."
  (unless (and (stringp prompt)
               (not (string-empty-p (string-trim prompt))))
    (user-error "Zara prompt must not be empty"))
  (append
   (zara-conversation--endpoint-arguments)
   (list "--conversation-id"
         (zara-conversation--validate-id conversation-id "conversation id")
         "--json-events"
         prompt)))

(defun zara-conversation--cancel-arguments (turn-id)
  "Return canonical CancelTurn CLI arguments for TURN-ID."
  (append
   (zara-conversation--endpoint-arguments)
   (list "--cancel-turn"
         (zara-conversation--validate-id turn-id "turn id"))))

(defun zara-conversation--current-request-p (process)
  "Return non-nil when PROCESS still owns the target buffer generation."
  (let ((target (process-get process 'zara-target))
        (generation (process-get process 'zara-generation)))
    (and (buffer-live-p target)
         (with-current-buffer target
           (= generation zara-conversation--generation)))))

(defun zara-conversation--finish-buffer (process state)
  "Finish PROCESS presentation state as STATE when it still owns the buffer."
  (let ((target (process-get process 'zara-target)))
    (when (and (buffer-live-p target)
               (zara-conversation--current-request-p process))
      (with-current-buffer target
        (setq zara-chat--busy nil
              zara-conversation--state state
              zara-conversation--request-process nil)
        (force-mode-line-update t)))))

(defun zara-conversation--deliver (process response error)
  "Deliver RESPONSE or ERROR once for PROCESS."
  (unless (process-get process 'zara-delivered)
    (process-put process 'zara-delivered t)
    (when (zara-conversation--current-request-p process)
      (let ((callback (process-get process 'zara-callback)))
        (when callback
          (funcall callback response error))))))

(defun zara-conversation--protocol-error (process message)
  "Fence PROCESS after protocol MESSAGE without accepting late output."
  (unless (process-get process 'zara-protocol-error)
    (process-put process 'zara-protocol-error message)
    (zara-conversation--deliver process nil message)
    (zara-conversation--finish-buffer process 'error))
  nil)

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

(defun zara-conversation--start-cancel (request-process turn-id)
  "Submit canonical CancelTurn for REQUEST-PROCESS and TURN-ID."
  (unless (process-get request-process 'zara-cancel-process)
    (let* ((program (zara--program))
           (stderr (generate-new-buffer " *zara-emacs-cancel-stderr*"))
           (cancel
            (make-process
             :name "zara-emacs-cancel"
             :command (cons program (zara-conversation--cancel-arguments turn-id))
             :buffer nil
             :stderr stderr
             :connection-type 'pipe
             :noquery t
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (let* ((request (process-get process 'zara-request-process))
                        (status (process-exit-status process))
                        (error-text
                         (when (buffer-live-p stderr)
                           (with-current-buffer stderr
                             (string-trim (buffer-string))))))
                   (when (buffer-live-p stderr)
                     (kill-buffer stderr))
                   (if (zerop status)
                       (progn
                         (process-put request 'zara-cancel-confirmed t)
                         (zara-conversation--finish-buffer request 'cancelled))
                     (let ((message
                            (if (string-empty-p (or error-text ""))
                                (format "CancelTurn failed with exit status %s" status)
                              error-text)))
                       (zara-conversation--protocol-error request message))))))))))
      (process-put cancel 'zara-request-process request-process)
      (process-put request-process 'zara-cancel-process cancel)))
  request-process)

(defun zara-conversation--accept-turn (process event)
  "Handle one turn.accepted EVENT for PROCESS."
  (let* ((expected-conversation (process-get process 'zara-conversation-id))
         (conversation (zara-conversation--event-string event "conversation_id"))
         (turn-id (zara-conversation--event-string event "turn_id"))
         (existing (process-get process 'zara-turn-id)))
    (unless (string= conversation expected-conversation)
      (error "native-client event conversation_id mismatch"))
    (zara-conversation--validate-id turn-id "turn id")
    (when (and existing (not (string= existing turn-id)))
      (error "native-client emitted conflicting turn ids"))
    (process-put process 'zara-turn-id turn-id)
    (when (process-get process 'zara-cancel-requested)
      (zara-conversation--start-cancel process turn-id))))

(defun zara-conversation--complete-turn (process event)
  "Handle one assistant.complete EVENT for PROCESS."
  (let* ((expected-conversation (process-get process 'zara-conversation-id))
         (conversation (zara-conversation--event-string event "conversation_id"))
         (turn-id (zara-conversation--event-string event "turn_id"))
         (accepted (process-get process 'zara-turn-id))
         (text (gethash "text" event)))
    (unless (string= conversation expected-conversation)
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

(defun zara-conversation--request-error-text (process)
  "Return bounded stderr diagnostics for PROCESS."
  (let ((stderr (process-get process 'zara-stderr)))
    (if (buffer-live-p stderr)
        (with-current-buffer stderr
          (string-trim (buffer-string)))
      "")))

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
       ((process-get process 'zara-protocol-error) nil)
       ((process-get process 'zara-cancel-requested)
        (unless (process-get process 'zara-cancel-confirmed)
          ;; The canonical cancel subprocess owns terminal cancellation state.
          ;; Keep late request output fenced while that command is pending.
          nil))
       ((not (zerop status))
        (let ((message (zara-conversation--request-error-text process)))
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
must emit the canonical `turn.accepted' then `assistant.complete' NDJSON
sequence.  Malformed, mismatched, duplicate, or incomplete events fail closed."
  (when zara-chat--busy
    (user-error "Zara is already handling a request"))
  (let* ((program (zara--program))
         (conversation-id (zara-conversation--current-id))
         (generation (cl-incf zara-conversation--generation))
         (stderr (generate-new-buffer " *zara-emacs-conversation-stderr*"))
         (process
          (make-process
           :name "zara-emacs-conversation"
           :command
           (cons program
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
    ;; Fence presentation immediately.  The request process still parses the
    ;; eventual turn.accepted receipt so cancellation can target the runtime-
    ;; minted turn id even when the user cancels before acceptance arrives.
    (cl-incf zara-conversation--generation)
    (process-put process 'zara-cancel-requested t)
    (when-let ((turn-id (process-get process 'zara-turn-id)))
      (zara-conversation--start-cancel process turn-id))
    (force-mode-line-update t)
    process))

;;;###autoload
(defun zara-conversation-switch (conversation-id)
  "Switch the Emacs surface to canonical CONVERSATION-ID while idle."
  (interactive (list (read-string "Zara conversation id: "
                                  (or zara-conversation-id
                                      zara-conversation-default-id))))
  (when zara-chat--busy
    (user-error "Cancel or finish the active Zara turn before switching"))
  (setq-local zara-conversation-id
              (zara-conversation--validate-id conversation-id "conversation id"))
  zara-conversation-id)

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
    map)
  "Keymap for `zara-conversation-mode'.")

;;;###autoload
(define-minor-mode zara-conversation-mode
  "Use canonical streaming conversation identity and cancellation in Zara chat.

The mode is presentation-only.  It does not create a provider, model runtime,
expert registry, planner, tool authority, or conversation-history store."
  :lighter " ZaraConv"
  :keymap zara-conversation-mode-map
  (when zara-conversation-mode
    (zara-conversation--current-id)))

;;;###autoload
(defun zara-conversation-chat ()
  "Open `zara-chat' with canonical conversation control enabled."
  (interactive)
  (zara-chat)
  (zara-conversation-mode 1))

(provide 'zara-conversation)
;;; zara-conversation.el ends here
