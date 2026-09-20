;;; zara-conversation-symbolic.el --- Persisted symbolic state for Zara chat -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (zara "0.2.0"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; Read-only presentation adapter for Zara's canonical persisted symbolic
;; conversation projection.  This module never owns history or symbolic state;
;; it asks Zara's ConversationStore-backed Python reader for one snapshot and
;; keeps only an ephemeral buffer-local copy for inspection.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'zara-conversation)

(defgroup zara-conversation-symbolic nil
  "Persisted symbolic conversation inspection for Zara Emacs chat."
  :group 'zara-conversation
  :prefix "zara-conversation-symbolic-")

(defcustom zara-conversation-symbolic-python-program "python3"
  "Python executable used for the local canonical projection reader."
  :type 'string
  :group 'zara-conversation-symbolic)

(defconst zara-conversation-symbolic--version "ZARA-SYMBOLIC-REPLAY/1")
(defvar-local zara-conversation-symbolic-projection nil)

(defun zara-conversation-symbolic--exact-integer (object key minimum)
  "Return exact integer KEY from OBJECT with lower bound MINIMUM."
  (let ((value (gethash key object :missing)))
    (unless (and (integerp value) (>= value minimum))
      (error "symbolic replay %s must be an integer >= %d" key minimum))
    value))

(defun zara-conversation-symbolic--array (object key)
  "Return JSON array KEY from OBJECT or signal an error."
  (let ((value (gethash key object :missing)))
    (unless (listp value)
      (error "symbolic replay %s must be a JSON array" key))
    value))

(defun zara-conversation-symbolic--validate-projection (projection)
  "Validate canonical persisted symbolic PROJECTION for presentation use."
  (when (eq projection :null)
    (setq projection nil))
  (when projection
    (unless (hash-table-p projection)
      (error "symbolic replay projection must be null or a JSON object"))
    (zara-conversation-symbolic--exact-integer projection "projection_generation" 1)
    (zara-conversation-symbolic--exact-integer projection "runtime_generation" 0)
    (zara-conversation-symbolic--exact-integer projection "project_generation" 0)
    (zara-conversation-symbolic--exact-integer projection "max_model_calls" 0)
    (zara-conversation-symbolic--exact-integer projection "provider_calls" 0)
    (zara-conversation-symbolic--exact-integer projection "model_calls" 0)
    (let ((providers-enabled (gethash "providers_enabled" projection :missing))
          (turn-id (gethash "turn_id" projection :missing))
          (project-id (gethash "project_id" projection :missing))
          (outcome (gethash "outcome" projection :missing))
          (dialogue-act (gethash "dialogue_act" projection :missing))
          (renderer (gethash "renderer_provenance" projection :missing))
          (dialogue-state (gethash "dialogue_state" projection :missing)))
      (unless (memq providers-enabled '(t :false))
        (error "symbolic replay providers_enabled must be boolean"))
      (unless (or (eq turn-id :null) (stringp turn-id))
        (error "symbolic replay turn_id must be null or a string"))
      (unless (or (eq project-id :null) (stringp project-id))
        (error "symbolic replay project_id must be null or a string"))
      (unless (member outcome '("unknown" "pending" "success" "cancelled" "interrupted" "error"))
        (error "symbolic replay outcome is invalid"))
      (unless (and (stringp dialogue-act) (not (string-empty-p dialogue-act)))
        (error "symbolic replay dialogue_act must be a non-empty string"))
      (unless (hash-table-p dialogue-state)
        (error "symbolic replay dialogue_state must be a JSON object"))
      (unless (member renderer '("" "symbolic-dcg/v1"))
        (error "symbolic replay renderer_provenance is invalid"))
      (when (and (string= outcome "success")
                 (not (string= renderer "symbolic-dcg/v1")))
        (error "successful symbolic replay requires symbolic-dcg/v1 renderer")))
    (dolist (key '("discourse_entities" "unresolved_questions" "expert_evidence"
                   "verified_facts" "verified_outcome_refs"))
      (zara-conversation-symbolic--array projection key))
    (unless (stringp (gethash "updated_at" projection :missing))
      (error "symbolic replay updated_at must be a string"))
    (let ((model-calls (gethash "model_calls" projection))
          (max-model-calls (gethash "max_model_calls" projection)))
      (when (> model-calls max-model-calls)
        (error "symbolic replay model_calls exceeds max_model_calls"))))
  projection)

(defun zara-conversation-symbolic--parse (text expected-conversation-id)
  "Parse symbolic replay TEXT for EXPECTED-CONVERSATION-ID."
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
      (error "symbolic replay payload must be a JSON object"))
    (unless (equal (gethash "version" payload) zara-conversation-symbolic--version)
      (error "unsupported symbolic replay version"))
    (unless (equal (gethash "conversation_id" payload) expected)
      (error "symbolic replay conversation id mismatch"))
    (zara-conversation-symbolic--validate-projection
     (gethash "symbolic_projection" payload :missing))))

(defun zara-conversation-symbolic--program ()
  "Resolve the configured Python executable or fail closed."
  (or (executable-find zara-conversation-symbolic-python-program)
      (user-error "Cannot find %s" zara-conversation-symbolic-python-program)))

(defun zara-conversation-symbolic--arguments (conversation-id)
  "Return local read-only projection-reader arguments for CONVERSATION-ID."
  (list "-m" "zara.desktop.conversation.replay_status"
        "--conversation-id"
        (zara-conversation--validate-id conversation-id "conversation id")))

;;;###autoload
(defun zara-conversation-symbolic-refresh-status ()
  "Refresh ephemeral symbolic status from Zara's canonical durable store."
  (interactive)
  (when zara-chat--busy
    (user-error "Cancel or finish the active Zara turn before refreshing status"))
  (let* ((conversation-id (zara-conversation--current-id))
         (program (zara-conversation-symbolic--program))
         (arguments (zara-conversation-symbolic--arguments conversation-id))
         projection)
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil arguments)))
        (unless (and (integerp status) (zerop status))
          (user-error "Zara symbolic status failed with exit status %s" status))
        (setq projection
              (zara-conversation-symbolic--parse
               (buffer-string) conversation-id))))
    (setq-local zara-conversation-symbolic-projection projection)
    projection))

(defun zara-conversation-symbolic--projection-value (key)
  "Return KEY from the cached projection, normalizing JSON null to nil."
  (when zara-conversation-symbolic-projection
    (let ((value (gethash key zara-conversation-symbolic-projection)))
      (unless (eq value :null) value))))

;;;###autoload
(defun zara-conversation-symbolic-status (&optional refresh)
  "Return persisted symbolic conversation status.

With prefix argument REFRESH, read a fresh canonical projection first."
  (interactive "P")
  (when refresh
    (zara-conversation-symbolic-refresh-status))
  (let ((projection zara-conversation-symbolic-projection))
    (let ((status
           (list
            :conversation-id (zara-conversation--current-id)
            :project-id (zara-conversation-symbolic--projection-value "project_id")
            :project-generation (and projection (gethash "project_generation" projection))
            :dialogue-act (zara-conversation-symbolic--projection-value "dialogue_act")
            :outcome (zara-conversation-symbolic--projection-value "outcome")
            :unresolved-question-count
            (and projection (length (gethash "unresolved_questions" projection)))
            :expert-evidence-count
            (and projection (length (gethash "expert_evidence" projection)))
            :verified-fact-count
            (and projection (length (gethash "verified_facts" projection)))
            :renderer-provenance
            (zara-conversation-symbolic--projection-value "renderer_provenance")
            :providers-enabled
            (and projection (gethash "providers_enabled" projection))
            :max-model-calls
            (and projection (gethash "max_model_calls" projection))
            :provider-calls
            (and projection (gethash "provider_calls" projection))
            :model-calls
            (and projection (gethash "model_calls" projection)))))
      (when (called-interactively-p 'interactive)
        (message "Zara symbolic %s · project %s · act %s · model %s/%s"
                 (or (plist-get status :outcome) "none")
                 (or (plist-get status :project-id) "none")
                 (or (plist-get status :dialogue-act) "none")
                 (or (plist-get status :model-calls) "-")
                 (or (plist-get status :max-model-calls) "-")))
      status)))

;;;###autoload
(defun zara-conversation-symbolic-replay ()
  "Replay canonical transcript, then refresh persisted symbolic presentation state."
  (interactive)
  (zara-conversation-replay)
  (zara-conversation-symbolic-refresh-status))

(defun zara-conversation-symbolic--clear-after-switch (&rest _ignored)
  "Fence cached project/discourse status after a conversation switch."
  (setq-local zara-conversation-symbolic-projection nil))

(advice-add 'zara-conversation-switch :after
            #'zara-conversation-symbolic--clear-after-switch)

(provide 'zara-conversation-symbolic)
;;; zara-conversation-symbolic.el ends here
