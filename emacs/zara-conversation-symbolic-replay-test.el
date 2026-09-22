;;; zara-conversation-symbolic-replay-test.el --- Symbolic replay continuity tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'zara)
(require 'zara-conversation)
(require 'zara-conversation-symbolic)

(defconst zara-conversation-symbolic-replay-test--payload
  (concat
   "{\"conversation_id\":\"emacs-main\","
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
   "\"version\":\"ZARA-SYMBOLIC-REPLAY/1\"}"))

(ert-deftest zara-conversation-symbolic-replay-restores-status-view ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let (captured)
      (cl-letf (((symbol-function 'zara-conversation-symbolic--program)
                 (lambda () "python3"))
                ((symbol-function 'process-file)
                 (lambda (program _infile _destination _display &rest args)
                   (setq captured (cons program args))
                   (insert zara-conversation-symbolic-replay-test--payload)
                   0)))
        (zara-conversation-symbolic-refresh-status))
      (should
       (equal captured
              '("python3" "-m" "zara.desktop.conversation.replay_status"
                "--conversation-id" "emacs-main"))))
    (let ((status (zara-conversation-symbolic-status)))
      (should (equal (plist-get status :project-id) "dotfiles"))
      (should (= (plist-get status :project-generation) 3))
      (should (equal (plist-get status :dialogue-act) "inform"))
      (should (= (plist-get status :unresolved-question-count) 1))
      (should (= (plist-get status :expert-evidence-count) 1))
      (should (= (plist-get status :verified-fact-count) 1))
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
     (zara-conversation-symbolic--parse payload "emacs-main")
     :type 'error)))

(ert-deftest zara-conversation-symbolic-replay-rejects-nonsymbolic-success-renderer ()
  (let ((payload
         (replace-regexp-in-string
          "symbolic-dcg/v1"
          "model-fallback/openrouter"
          zara-conversation-symbolic-replay-test--payload
          t t)))
    (should-error
     (zara-conversation-symbolic--parse payload "emacs-main")
     :type 'error)))

(ert-deftest zara-conversation-symbolic-replay-keeps-one-canonical-snapshot ()
  "Transcript replay and symbolic status must come from the same canonical snapshot."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let ((replay-calls 0)
          (refresh-calls 0))
      (cl-letf (((symbol-function 'zara-conversation-replay)
                 (lambda ()
                   (cl-incf replay-calls)
                   (setq-local
                    zara-conversation-symbolic-projection
                    (zara-conversation-symbolic--parse
                     zara-conversation-symbolic-replay-test--payload
                     "emacs-main"))
                   :canonical-replay))
                ((symbol-function 'zara-conversation-symbolic-refresh-status)
                 (lambda ()
                   (cl-incf refresh-calls)
                   (error "symbolic replay must not issue a second canonical read"))))
        (let ((projection (zara-conversation-symbolic-replay)))
          (should (eq projection zara-conversation-symbolic-projection))
          (should (= replay-calls 1))
          (should (= refresh-calls 0))
          (should (equal (gethash "project_id" projection) "dotfiles"))
          (should (= (gethash "project_generation" projection) 3))
          (should (= (length (gethash "expert_evidence" projection)) 1))
          (should (eq (gethash "providers_enabled" projection) :false))
          (should (= (gethash "max_model_calls" projection) 0))
          (should (= (gethash "provider_calls" projection) 0))
          (should (= (gethash "model_calls" projection) 0)))))))

(ert-deftest zara-conversation-symbolic-switch-fences-stale-project-status ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-symbolic-projection
                (zara-conversation-symbolic--parse
                 zara-conversation-symbolic-replay-test--payload
                 "emacs-main"))
    (zara-conversation-switch "other-project-chat")
    (should-not zara-conversation-symbolic-projection)))

(provide 'zara-conversation-symbolic-replay-test)
;;; zara-conversation-symbolic-replay-test.el ends here
