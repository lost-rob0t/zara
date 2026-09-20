;;; zara-conversation-symbolic-replay-test.el --- Symbolic replay continuity tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'zara)
(require 'zara-conversation)

(defconst zara-conversation-symbolic-replay-test--payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-20T18:00:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-20T18:05:00\"},"
   "\"messages\":[],"
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

(ert-deftest zara-conversation-symbolic-replay-restores-status-view ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let ((payload
           (zara-conversation--parse-replay
            zara-conversation-symbolic-replay-test--payload
            "emacs-main")))
      (zara-conversation--render-replay payload))
    (let ((status (zara-conversation-status)))
      (should (equal (plist-get status :project-id) "dotfiles"))
      (should (= (plist-get status :project-generation) 3))
      (should (equal (plist-get status :dialogue-act) "inform"))
      (should (= (plist-get status :unresolved-question-count) 1))
      (should (eq (plist-get status :providers-enabled) :false))
      (should (= (plist-get status :max-model-calls) 0))
      (should (= (plist-get status :provider-calls) 0))
      (should (= (plist-get status :model-calls) 0)))))

(ert-deftest zara-conversation-symbolic-replay-rejects-malformed-policy-state ()
  (let ((payload
         (replace-regexp-in-string
          "\"max_model_calls\":0"
          "\"max_model_calls\":false"
          zara-conversation-symbolic-replay-test--payload
          t t)))
    (should-error
     (zara-conversation--parse-replay payload "emacs-main")
     :type 'error)))

(ert-deftest zara-conversation-symbolic-replay-rejects-nonsymbolic-success-renderer ()
  (let ((payload
         (replace-regexp-in-string
          "symbolic-dcg/v1"
          "model-fallback/openrouter"
          zara-conversation-symbolic-replay-test--payload
          t t)))
    (should-error
     (zara-conversation--parse-replay payload "emacs-main")
     :type 'error)))

(provide 'zara-conversation-symbolic-replay-test)
;;; zara-conversation-symbolic-replay-test.el ends here
