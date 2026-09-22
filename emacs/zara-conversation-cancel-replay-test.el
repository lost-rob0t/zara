;;; zara-conversation-cancel-replay-test.el --- Cancellation/replay continuity tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'zara)
(require 'zara-conversation)
(require 'zara-conversation-symbolic)

(defconst zara-conversation-cancel-replay-test--payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-22T19:30:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-22T19:31:00\"},"
   "\"messages\":["
   "{\"content\":\"start a timer\",\"error\":\"\",\"role\":\"user\","
   "\"sequence\":1,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-cancel\"},"
   "{\"content\":\"cancelled by operator\",\"error\":\"\",\"role\":\"assistant\","
   "\"sequence\":2,\"status\":\"cancelled\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-cancel\"}],"
   "\"symbolic_projection\":{"
   "\"dialogue_act\":\"cancel\","
   "\"dialogue_state\":{\"topic\":\"timer\"},"
   "\"discourse_entities\":[],\"expert_evidence\":[],"
   "\"max_model_calls\":0,\"model_calls\":0,\"outcome\":\"cancelled\","
   "\"project_generation\":3,\"project_id\":\"dotfiles\","
   "\"projection_generation\":10,\"provider_calls\":0,"
   "\"providers_enabled\":false,\"renderer_provenance\":\"\","
   "\"runtime_generation\":8,\"turn_id\":\"turn-cancel\","
   "\"unresolved_questions\":[],"
   "\"updated_at\":\"2026-09-22T19:31:00\","
   "\"verified_facts\":[],\"verified_outcome_refs\":[]},"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(defconst zara-conversation-cancel-replay-test--verified-effect-payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-22T19:30:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-22T19:35:00\"},"
   "\"messages\":["
   "{\"content\":\"start a timer\",\"error\":\"\",\"role\":\"user\","
   "\"sequence\":1,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-cancel\"},"
   "{\"content\":\"cancelled by operator\",\"error\":\"\",\"role\":\"assistant\","
   "\"sequence\":2,\"status\":\"cancelled\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-cancel\"},"
   "{\"content\":\"open Firefox\",\"error\":\"\",\"role\":\"user\","
   "\"sequence\":3,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-effect\"},"
   "{\"content\":\"Firefox opened and verified.\",\"error\":\"\","
   "\"role\":\"assistant\",\"sequence\":4,\"status\":\"complete\","
   "\"tool_run_id\":\"tool-run-9\",\"turn_id\":\"turn-effect\"}],"
   "\"symbolic_projection\":{"
   "\"dialogue_act\":\"verified\","
   "\"dialogue_state\":{\"intent\":\"open_app\",\"target\":\"firefox\"},"
   "\"discourse_entities\":[],\"expert_evidence\":[],"
   "\"max_model_calls\":0,\"model_calls\":0,\"outcome\":\"success\","
   "\"project_generation\":3,\"project_id\":\"dotfiles\","
   "\"projection_generation\":11,\"provider_calls\":0,"
   "\"providers_enabled\":false,\"renderer_provenance\":\"symbolic-dcg/v1\","
   "\"runtime_generation\":9,\"turn_id\":\"turn-effect\","
   "\"unresolved_questions\":[],"
   "\"updated_at\":\"2026-09-22T19:35:00\","
   "\"verified_facts\":[],"
   "\"verified_outcome_refs\":["
   "\"zara.verified-outcome/v2:9:outcome:postcondition/process-firefox\"]},"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(defun zara-conversation-cancel-replay-test--process (target callback)
  "Create one inert request process owned by TARGET with CALLBACK."
  (let ((process
         (make-pipe-process
          :name (generate-new-buffer-name "zara-cancel-replay-test")
          :buffer nil
          :noquery t)))
    (process-put process 'zara-target target)
    (process-put process 'zara-generation 1)
    (process-put process 'zara-conversation-id "emacs-main")
    (process-put process 'zara-callback callback)
    (process-put process 'zara-line-buffer "")
    (with-current-buffer target
      (zara-chat-mode)
      (setq-local zara-conversation-id "emacs-main")
      (setq-local zara-conversation--generation 1)
      (setq-local zara-conversation--request-process process)
      (setq-local zara-conversation--state 'running)
      (setq-local zara-chat--busy t))
    process))

(defun zara-conversation-cancel-replay-test--accepted ()
  "Return the canonical acceptance event for the cancellation fixture."
  (json-serialize
   '((type . "turn.accepted")
     (conversation_id . "emacs-main")
     (turn_id . "turn-cancel"))))

(defun zara-conversation-cancel-replay-test--complete ()
  "Return a late completion that must never reach the Emacs presentation."
  (json-serialize
   '((type . "assistant.complete")
     (conversation_id . "emacs-main")
     (turn_id . "turn-cancel")
     (text . "stale symbolic output"))))

(ert-deftest zara-conversation-cancelled-generation-replays-canonical-state-only ()
  "Cancel fences late output, then replay restores only durable cancelled state."
  (let ((target (generate-new-buffer " *zara-cancel-replay-target*"))
        response
        error
        cancelled-turn
        process)
    (unwind-protect
        (progn
          (setq process
                (zara-conversation-cancel-replay-test--process
                 target
                 (lambda (value failure)
                   (setq response value
                         error failure))))
          (cl-letf (((symbol-function 'zara-conversation--start-cancel)
                     (lambda (request turn-id)
                       (setq cancelled-turn turn-id)
                       (process-put request 'zara-cancel-confirmed t)
                       (zara-conversation--finish-buffer request 'cancelled)
                       request)))
            (with-current-buffer target
              (zara-conversation-cancel)
              (should (= zara-conversation--generation 2))
              (should (eq zara-conversation--state 'cancelling)))
            (zara-conversation--handle-line
             process
             (zara-conversation-cancel-replay-test--accepted))
            (should (equal cancelled-turn "turn-cancel"))
            (zara-conversation--handle-line
             process
             (zara-conversation-cancel-replay-test--complete)))
          (should-not response)
          (should-not error)
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'cancelled))
            (should-not zara-conversation--request-process)
            (setq-local zara-conversation-context-ids
                        '("evidence:stale" "file:old"))
            (let ((reads 0)
                  captured)
              (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
                        ((symbol-function 'process-file)
                         (lambda (program _infile _destination _display &rest args)
                           (cl-incf reads)
                           (setq captured (cons program args))
                           (insert zara-conversation-cancel-replay-test--payload)
                           0)))
                (zara-conversation-replay))
              (should (= reads 1))
              (should
               (equal captured
                      '("zara" "--replay-conversation" "emacs-main"))))
            (should-not zara-conversation-context-ids)
            (should-not
             (string-match-p "stale symbolic output" (buffer-string)))
            (should
             (string-match-p "cancelled by operator" (buffer-string)))
            (let ((status (zara-conversation-symbolic-status)))
              (should (equal (plist-get status :turn-id) "turn-cancel"))
              (should (equal (plist-get status :outcome) "cancelled"))
              (should (eq (plist-get status :providers-enabled) :false))
              (should (= (plist-get status :max-model-calls) 0))
              (should (= (plist-get status :provider-calls) 0))
              (should (= (plist-get status :model-calls) 0)))
            (setq-local zara-conversation-context-ids
                        '("evidence:replay-current" "file:timer-current"))
            (let ((zara-connect-endpoint nil))
              (should
               (equal
                (zara-conversation--turn-arguments
                 zara-conversation-id
                 "what should I do instead?")
                '("--conversation-id" "emacs-main"
                  "--context-id" "evidence:replay-current"
                  "--context-id" "file:timer-current"
                  "--json-events" "what should I do instead?"))))
            (should-not
             (member "evidence:stale"
                     (zara-conversation--context-arguments)))
            (should-not
             (member "file:old"
                     (zara-conversation--context-arguments)))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-cancelled-replay-continues-with-fresh-verified-effect ()
  "A post-cancel success replays only with fresh generation-bound postcondition evidence."
  (let ((target (generate-new-buffer " *zara-cancel-effect-target*")))
    (unwind-protect
        (with-current-buffer target
          (zara-chat-mode)
          (setq-local zara-conversation-id "emacs-main")
          (setq-local zara-conversation-context-ids
                      '("zara.verified-outcome/v2:8:outcome:postcondition/stale-effect"))
          (let ((reads 0)
                captured)
            (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
                      ((symbol-function 'process-file)
                       (lambda (program _infile _destination _display &rest args)
                         (cl-incf reads)
                         (setq captured (cons program args))
                         (insert zara-conversation-cancel-replay-test--verified-effect-payload)
                         0)))
              (zara-conversation-replay))
            (should (= reads 1))
            (should
             (equal captured
                    '("zara" "--replay-conversation" "emacs-main"))))
          (should-not zara-conversation-context-ids)
          (should
           (string-match-p "cancelled by operator" (buffer-string)))
          (should
           (string-match-p "Firefox opened and verified" (buffer-string)))
          (let* ((status (zara-conversation-symbolic-status))
                 (fresh-ref
                  "zara.verified-outcome/v2:9:outcome:postcondition/process-firefox"))
            (should (equal (plist-get status :turn-id) "turn-effect"))
            (should (equal (plist-get status :outcome) "success"))
            (should (equal (plist-get status :dialogue-act) "verified"))
            (should
             (equal (plist-get status :verified-outcome-refs)
                    (list fresh-ref)))
            (should (eq (plist-get status :providers-enabled) :false))
            (should (= (plist-get status :max-model-calls) 0))
            (should (= (plist-get status :provider-calls) 0))
            (should (= (plist-get status :model-calls) 0))
            (setq-local zara-conversation-context-ids (list fresh-ref))
            (let ((zara-connect-endpoint nil))
              (should
               (equal
                (zara-conversation--turn-arguments
                 zara-conversation-id
                 "why did that work?")
                (list "--conversation-id" "emacs-main"
                      "--context-id" fresh-ref
                      "--json-events" "why did that work?"))))
            (should-not
             (member
              "zara.verified-outcome/v2:8:outcome:postcondition/stale-effect"
              (zara-conversation--context-arguments)))))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(provide 'zara-conversation-cancel-replay-test)
;;; zara-conversation-cancel-replay-test.el ends here
