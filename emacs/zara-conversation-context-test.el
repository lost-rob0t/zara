;;; zara-conversation-context-test.el --- Context refs for canonical Zara chat -*- lexical-binding: t; -*-

(require 'ert)
(require 'zara-conversation)

(ert-deftest zara-conversation-turn-arguments-forward-context-ids ()
  "Context refs stay ephemeral in Emacs and cross the canonical CLI boundary."
  (let ((zara-connect-endpoint nil)
        (zara-conversation-context-ids '(" doc:alpha " "project:zara")))
    (should
     (equal
      (zara-conversation--turn-arguments "emacs-main" "continue")
      '("--conversation-id" "emacs-main"
        "--context-id" "doc:alpha"
        "--context-id" "project:zara"
        "--json-events" "continue")))))

(ert-deftest zara-conversation-switch-clears-ephemeral-context-ids ()
  "A conversation switch cannot leak old context references into the new turn."
  (with-temp-buffer
    (setq-local zara-chat--busy nil)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("project:old" "doc:stale"))
    (should (equal (zara-conversation-switch "project-next") "project-next"))
    (should (equal zara-conversation-id "project-next"))
    (should-not zara-conversation-context-ids)))

(ert-deftest zara-conversation-switch-fences-visible-transcript ()
  "Switching conversations must not leave the previous transcript on screen."
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (setq-local zara-conversation-context-ids '("project:old"))
    (let ((inhibit-read-only t))
      (insert "You\nold-project-secret\n\nZara\nold-project-answer\n\n"))
    (should (equal (zara-conversation-switch "project-next") "project-next"))
    (should (equal zara-conversation-id "project-next"))
    (should-not zara-conversation-context-ids)
    (should-not (string-match-p "old-project-secret" (buffer-string)))
    (should-not (string-match-p "old-project-answer" (buffer-string)))
    (should (string-match-p "Canonical conversation project-next" (buffer-string)))
    (should (string-match-p "replay" (downcase (buffer-string))))))

(ert-deftest zara-conversation-context-ids-fail-closed-over-budget ()
  "Emacs refuses an unbounded context set before starting the native client."
  (let ((zara-conversation-context-ids
         (cl-loop for index below 33 collect (format "doc:%d" index))))
    (should-error (zara-conversation--context-arguments) :type 'user-error)))

(provide 'zara-conversation-context-test)
;;; zara-conversation-context-test.el ends here
