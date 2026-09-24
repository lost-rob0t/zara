;;; zara-conversation-unified-replay-test.el --- Unified transcript/state replay tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'zara)
(require 'zara-conversation)
(require 'zara-conversation-symbolic)

(defconst zara-conversation-unified-replay-test--payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-20T18:00:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-20T18:05:00\"},"
   "\"messages\":["
   "{\"content\":\"hello\",\"error\":\"\",\"role\":\"user\","
   "\"sequence\":1,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-7\"}],"
   "\"symbolic_projection\":{"
   "\"dialogue_act\":\"inform\","
   "\"dialogue_state\":{\"topic\":\"nix-shell\"},"
   "\"discourse_entities\":[{\"id\":\"file:flake.nix\",\"kind\":\"file\"}],"
   "\"expert_evidence\":[{\"expert\":\"DotfilesExpert\",\"ref\":\"evidence:42\"}],"
   "\"max_model_calls\":0,\"model_calls\":0,\"outcome\":\"success\","
   "\"project_generation\":3,\"project_id\":\"dotfiles\","
   "\"projection_generation\":9,\"provider_calls\":0,"
   "\"providers_enabled\":false,\"renderer_provenance\":\"symbolic-dcg/v1\","
   "\"runtime_generation\":7,\"turn_id\":\"turn-7\","
   "\"unresolved_questions\":[{\"id\":\"q:1\",\"text\":\"which profile?\"}],"
   "\"updated_at\":\"2026-09-20T18:05:00\","
   "\"verified_facts\":[{\"fact\":\"project uses flakes\",\"ref\":\"fact:9\"}],"
   "\"verified_outcome_refs\":[\"zara.verified-outcome/v1:outcome:turn-7\"]},"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(defconst zara-conversation-unified-replay-test--project-b-payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-22T15:30:00\","
   "\"id\":\"emacs-project-b\",\"title\":\"Project B\","
   "\"updated_at\":\"2026-09-22T15:31:00\"},"
   "\"messages\":["
   "{\"content\":\"project B answer\",\"error\":\"\",\"role\":\"assistant\","
   "\"sequence\":1,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-b\"}],"
   "\"symbolic_projection\":{"
   "\"dialogue_act\":\"inform\","
   "\"dialogue_state\":{\"topic\":\"project-b\"},"
   "\"discourse_entities\":[{\"id\":\"project:b\",\"kind\":\"project\"}],"
   "\"expert_evidence\":[{\"expert\":\"ProjectBExpert\",\"ref\":\"evidence:b\"}],"
   "\"max_model_calls\":0,\"model_calls\":0,\"outcome\":\"success\","
   "\"project_generation\":5,\"project_id\":\"project-b\","
   "\"projection_generation\":12,\"provider_calls\":0,"
   "\"providers_enabled\":false,\"renderer_provenance\":\"symbolic-dcg/v1\","
   "\"runtime_generation\":9,\"turn_id\":\"turn-b\","
   "\"unresolved_questions\":[],"
   "\"updated_at\":\"2026-09-22T15:31:00\","
   "\"verified_facts\":[{\"fact\":\"project B selected\",\"ref\":\"fact:b\"}],"
   "\"verified_outcome_refs\":[\"zara.verified-outcome/v1:outcome:turn-b\"]},"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(defconst zara-conversation-unified-replay-test--null-projection-payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-20T18:00:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-20T18:05:00\"},"
   "\"messages\":[],\"symbolic_projection\":null,"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(ert-deftest zara-conversation-replay-restores-transcript-and-symbolic-state-from-one-read ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-symbolic-projection nil)
    (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
              ((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (insert zara-conversation-unified-replay-test--payload)
                 0)))
      (zara-conversation-replay))
    (should (string-match-p "Canonical conversation emacs-main" (buffer-string)))
    (should zara-conversation-symbolic-projection)
    (let ((status (zara-conversation-symbolic-status)))
      (should (equal (plist-get status :project-id) "dotfiles"))
      (should (= (plist-get status :project-generation) 3))
      (should (equal (plist-get status :dialogue-act) "inform"))
      (should (= (plist-get status :unresolved-question-count) 1))
      (should (= (plist-get status :model-calls) 0))
      (should (= (plist-get status :max-model-calls) 0))
      (should (eq (plist-get status :providers-enabled) :false)))))

(ert-deftest zara-conversation-replay-null-projection-clears-stale-symbolic-state ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-symbolic-projection (make-hash-table :test 'equal))
    (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
              ((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (insert zara-conversation-unified-replay-test--null-projection-payload)
                 0)))
      (zara-conversation-replay))
    (should-not zara-conversation-symbolic-projection)
    (should (string-match-p "Canonical conversation emacs-main" (buffer-string)))))

(ert-deftest zara-conversation-replay-invalid-symbolic-state-is-presentation-atomic ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let ((inhibit-read-only t))
      (insert "existing presentation"))
    (let* ((before (buffer-string))
           (previous (make-hash-table :test 'equal))
           (payload
            (replace-regexp-in-string
             "\"providers_enabled\":false"
             "\"providers_enabled\":true"
             zara-conversation-unified-replay-test--payload
             t t)))
      (setq-local zara-conversation-symbolic-projection previous)
      (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
                ((symbol-function 'process-file)
                 (lambda (_program _infile _destination _display &rest _args)
                   (insert payload)
                   0)))
        (should-error (zara-conversation-replay) :type 'error))
      (should (equal (buffer-string) before))
      (should (eq zara-conversation-symbolic-projection previous)))))

(ert-deftest zara-conversation-switch-then-replay-keeps-project-context-fenced ()
  "Replay after a project switch uses one current-project snapshot and no stale refs."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("context:flake.nix" "context:host"))
    (setq-local zara-conversation-symbolic-projection (make-hash-table :test 'equal))
    (let ((inhibit-read-only t))
      (insert "stale project A transcript"))
    (should (equal (zara-conversation-switch "emacs-project-b") "emacs-project-b"))
    (should (null zara-conversation-context-ids))
    (should (null zara-conversation-symbolic-projection))
    (let ((read-count 0))
      (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
                ((symbol-function 'process-file)
                 (lambda (_program _infile _destination _display &rest args)
                   (cl-incf read-count)
                   (should (equal args '("--replay-conversation" "emacs-project-b")))
                   (insert zara-conversation-unified-replay-test--project-b-payload)
                   0)))
        (zara-conversation-replay))
      (should (= read-count 1)))
    (should (equal zara-conversation-id "emacs-project-b"))
    (should (null zara-conversation-context-ids))
    (should-not (string-match-p "stale project A transcript" (buffer-string)))
    (should (string-match-p "Canonical conversation emacs-project-b" (buffer-string)))
    (should (string-match-p "project B answer" (buffer-string)))
    (let* ((status (zara-conversation-symbolic-status))
           (evidence (plist-get status :expert-evidence)))
      (should (equal (plist-get status :conversation-id) "emacs-project-b"))
      (should (equal (plist-get status :project-id) "project-b"))
      (should (equal (gethash "expert" (car evidence)) "ProjectBExpert"))
      (should (eq (plist-get status :providers-enabled) :false))
      (should (= (plist-get status :max-model-calls) 0))
      (should (= (plist-get status :provider-calls) 0))
      (should (= (plist-get status :model-calls) 0)))))

(provide 'zara-conversation-unified-replay-test)
;;; zara-conversation-unified-replay-test.el ends here
