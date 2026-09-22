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

(defconst zara-conversation-symbolic-replay-test--canonical-payload
  (concat
   "{\"version\":\"ZARA-CONVERSATION-REPLAY/1\","
   "\"conversation\":{\"id\":\"emacs-main\",\"title\":\"Emacs Main\","
   "\"created_at\":\"2026-09-20T18:00:00\","
   "\"updated_at\":\"2026-09-20T18:05:00\"},"
   "\"messages\":[{\"sequence\":1,\"role\":\"assistant\","
   "\"content\":\"This project uses flakes.\",\"status\":\"complete\","
   "\"turn_id\":\"turn-7\",\"error\":\"\",\"tool_run_id\":null}],"
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
   "\"unresolved_questions\":[],\"updated_at\":\"2026-09-20T18:05:00\","
   "\"verified_facts\":[{\"fact\":\"project uses flakes\",\"ref\":\"fact:9\"}],"
   "\"verified_outcome_refs\":[\"zara.verified-outcome/v1:outcome:turn-7\"]}}"))

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

(ert-deftest zara-conversation-symbolic-replay-fences-stale-follow-up-context ()
  "A successful replay invalidates ephemeral refs from the older presentation snapshot."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("evidence:stale" "file:old"))
    (let (captured)
      (cl-letf (((symbol-function 'zara--program)
                 (lambda () "zara"))
                ((symbol-function 'process-file)
                 (lambda (program _infile _destination _display &rest args)
                   (setq captured (cons program args))
                   (insert zara-conversation-symbolic-replay-test--canonical-payload)
                   0)))
        (zara-conversation-replay))
      (should
       (equal captured
              '("zara" "--replay-conversation" "emacs-main"))))
    (should (string-match-p "This project uses flakes\\." (buffer-string)))
    (should-not zara-conversation-context-ids)
    (should (equal (gethash "project_id" zara-conversation-symbolic-projection)
                   "dotfiles"))
    (should (eq (gethash "providers_enabled" zara-conversation-symbolic-projection)
                :false))
    (should (= (gethash "max_model_calls" zara-conversation-symbolic-projection) 0))
    (should (= (gethash "provider_calls" zara-conversation-symbolic-projection) 0))
    (should (= (gethash "model_calls" zara-conversation-symbolic-projection) 0))
    (setq-local zara-conversation-context-ids '("evidence:42" "file:flake.nix"))
    (let ((zara-connect-endpoint nil))
      (should
       (equal
        (zara-conversation--turn-arguments
         zara-conversation-id
         "why does that apply here?")
        '("--conversation-id" "emacs-main"
          "--context-id" "evidence:42"
          "--context-id" "file:flake.nix"
          "--json-events" "why does that apply here?"))))))

(ert-deftest zara-conversation-symbolic-replay-failure-preserves-current-context ()
  "A failed replay must not erase refs from the still-visible presentation snapshot."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("evidence:current" "file:current"))
    (cl-letf (((symbol-function 'zara--program)
               (lambda () "zara"))
              ((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 9)))
      (should-error (zara-conversation-replay) :type 'user-error))
    (should
     (equal zara-conversation-context-ids
            '("evidence:current" "file:current")))))

(ert-deftest zara-conversation-symbolic-restart-replay-restores-current-evidence-only ()
  "A recreated Emacs surface reloads durable evidence without reviving stale refs."
  (let ((first (generate-new-buffer " *zara-symbolic-before-restart*"))
        second)
    (unwind-protect
        (progn
          (with-current-buffer first
            (zara-chat-mode)
            (setq-local zara-conversation-id "emacs-main")
            (setq-local zara-conversation-context-ids
                        '("evidence:stale" "file:old"))
            (setq-local
             zara-conversation-symbolic-projection
             (zara-conversation-symbolic--parse
              zara-conversation-symbolic-replay-test--payload
              "emacs-main")))
          ;; Buffer-local selectors and cached projection are presentation state.
          ;; Killing the surface models Emacs/process recreation; durable truth
          ;; must come back only through canonical replay.
          (kill-buffer first)
          (setq first nil)
          (setq second (generate-new-buffer " *zara-symbolic-after-restart*"))
          (with-current-buffer second
            (zara-chat-mode)
            (setq-local zara-conversation-id "emacs-main")
            (should-not zara-conversation-context-ids)
            (should-not zara-conversation-symbolic-projection)
            (let ((calls 0)
                  captured)
              (cl-letf (((symbol-function 'zara--program)
                         (lambda () "zara"))
                        ((symbol-function 'process-file)
                         (lambda (program _infile _destination _display &rest args)
                           (cl-incf calls)
                           (setq captured (cons program args))
                           (insert zara-conversation-symbolic-replay-test--canonical-payload)
                           0)))
                (let ((projection (zara-conversation-symbolic-replay)))
                  (should (= calls 1))
                  (should
                   (equal captured
                          '("zara" "--replay-conversation" "emacs-main")))
                  (should (eq projection zara-conversation-symbolic-projection))
                  (should (equal (gethash "project_id" projection) "dotfiles"))
                  (should (= (gethash "project_generation" projection) 3))
                  (should (= (length (gethash "expert_evidence" projection)) 1))
                  (should (eq (gethash "providers_enabled" projection) :false))
                  (should (= (gethash "max_model_calls" projection) 0))
                  (should (= (gethash "provider_calls" projection) 0))
                  (should (= (gethash "model_calls" projection) 0)))))
            (should (string-match-p "This project uses flakes\\." (buffer-string)))
            (should-not zara-conversation-context-ids)
            (setq-local zara-conversation-context-ids
                        '("evidence:42" "file:flake.nix"))
            (let ((zara-connect-endpoint nil))
              (should
               (equal
                (zara-conversation--turn-arguments
                 zara-conversation-id
                 "why does that apply here?")
                '("--conversation-id" "emacs-main"
                  "--context-id" "evidence:42"
                  "--context-id" "file:flake.nix"
                  "--json-events" "why does that apply here?"))))))
      (when (buffer-live-p first)
        (kill-buffer first))
      (when (buffer-live-p second)
        (kill-buffer second)))))

(provide 'zara-conversation-symbolic-replay-test)
;;; zara-conversation-symbolic-replay-test.el ends here
