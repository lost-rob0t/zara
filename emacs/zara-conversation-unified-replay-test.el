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

(provide 'zara-conversation-unified-replay-test)
;;; zara-conversation-unified-replay-test.el ends here
