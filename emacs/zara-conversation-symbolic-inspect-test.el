;;; zara-conversation-symbolic-inspect-test.el --- Symbolic inspection tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'zara)
(require 'zara-conversation)
(require 'zara-conversation-symbolic)

(defconst zara-conversation-symbolic-inspect-test--payload
  (concat
   "{\"conversation_id\":\"emacs-main\","
   "\"symbolic_projection\":{"
   "\"dialogue_act\":\"explain\","
   "\"dialogue_state\":{\"topic\":\"flake.nix\",\"follow_up\":\"why\"},"
   "\"discourse_entities\":[{\"id\":\"file:flake.nix\",\"kind\":\"file\"}],"
   "\"expert_evidence\":[{\"expert\":\"DotfilesExpert\",\"ref\":\"evidence:42\"},"
   "{\"expert\":\"NixExpert\",\"ref\":\"evidence:43\"}],"
   "\"max_model_calls\":0,\"model_calls\":0,\"outcome\":\"success\","
   "\"project_generation\":4,\"project_id\":\"dotfiles\","
   "\"projection_generation\":10,\"provider_calls\":0,"
   "\"providers_enabled\":false,\"renderer_provenance\":\"symbolic-dcg/v1\","
   "\"runtime_generation\":8,\"turn_id\":\"turn-8\","
   "\"unresolved_questions\":[{\"id\":\"q:2\",\"text\":\"which host?\"}],"
   "\"updated_at\":\"2026-09-21T04:25:00Z\","
   "\"verified_facts\":[{\"fact\":\"project uses flakes\",\"ref\":\"fact:9\"}],"
   "\"verified_outcome_refs\":[\"zara.verified-outcome/v1:outcome:turn-8\"]},"
   "\"version\":\"ZARA-SYMBOLIC-REPLAY/1\"}"))

(defun zara-conversation-symbolic-inspect-test--projection ()
  "Return one validated pure-symbolic inspection fixture."
  (zara-conversation-symbolic--parse
   zara-conversation-symbolic-inspect-test--payload
   "emacs-main"))

(ert-deftest zara-conversation-symbolic-status-exposes-canonical-evidence ()
  "Programmatic status exposes evidence, not only presentation counts."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-symbolic-projection
                (zara-conversation-symbolic-inspect-test--projection))
    (let* ((status (zara-conversation-symbolic-status))
           (evidence (plist-get status :expert-evidence))
           (question (car (plist-get status :unresolved-questions))))
      (should (equal (plist-get status :turn-id) "turn-8"))
      (should (equal (gethash "topic" (plist-get status :dialogue-state)) "flake.nix"))
      (should (= (length evidence) 2))
      (should (equal (gethash "expert" (car evidence)) "DotfilesExpert"))
      (should (equal (gethash "text" question) "which host?"))
      (should (= (length (plist-get status :verified-outcome-refs)) 1))
      (should (eq (plist-get status :providers-enabled) :false))
      (should (= (plist-get status :max-model-calls) 0))
      (should (= (plist-get status :provider-calls) 0))
      (should (= (plist-get status :model-calls) 0)))))

(ert-deftest zara-conversation-symbolic-inspect-renders-fresh-expert-state ()
  "Inspection renders a fresh canonical projection without another state owner."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let ((projection (zara-conversation-symbolic-inspect-test--projection))
          inspected
          (refresh-count 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'zara-conversation-symbolic-refresh-status)
                       (lambda ()
                         (cl-incf refresh-count)
                         (setq-local zara-conversation-symbolic-projection projection)
                         projection))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _ignored)
                         (setq inspected buffer)
                         buffer)))
              (should (bufferp (zara-conversation-symbolic-inspect))))
            (should (= refresh-count 1))
            (should (buffer-live-p inspected))
            (with-current-buffer inspected
              (should buffer-read-only)
              (let ((text (buffer-string)))
                (should (string-match-p "conversation_id=emacs-main" text))
                (should (string-match-p "providers_enabled=false" text))
                (should (string-match-p "max_model_calls=0" text))
                (should (string-match-p "provider_calls=0" text))
                (should (string-match-p "model_calls=0" text))
                (should (string-match-p "expert_evidence" text))
                (should (string-match-p "DotfilesExpert" text))
                (should (string-match-p "NixExpert" text))
                (should (string-match-p "unresolved_questions" text))
                (should (string-match-p "which host" text))
                (should (string-match-p "verified_outcome_refs" text)))))
        (when (buffer-live-p inspected)
          (kill-buffer inspected))))))

(ert-deftest zara-conversation-symbolic-inspect-refuses-busy-turn ()
  "A status refresh cannot race the active canonical turn presentation."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-chat--busy t)
    (should-error (zara-conversation-symbolic-inspect) :type 'user-error)))

(ert-deftest zara-conversation-symbolic-switch-fences-stale-project-evidence ()
  "Project/conversation switching cannot leak stale refs or symbolic evidence."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("context:flake.nix" "context:host"))
    (setq-local zara-conversation-symbolic-projection
                (zara-conversation-symbolic-inspect-test--projection))
    (should (equal (zara-conversation-switch "emacs-project-b") "emacs-project-b"))
    (should (equal zara-conversation-id "emacs-project-b"))
    (should (null zara-conversation-context-ids))
    (should (null zara-conversation-symbolic-projection))
    (let ((status (zara-conversation-symbolic-status)))
      (should (equal (plist-get status :conversation-id) "emacs-project-b"))
      (should (null (plist-get status :expert-evidence)))
      (should (null (plist-get status :dialogue-state)))
      (should (null (plist-get status :provider-calls)))
      (should (null (plist-get status :model-calls))))))

(provide 'zara-conversation-symbolic-inspect-test)
;;; zara-conversation-symbolic-inspect-test.el ends here
