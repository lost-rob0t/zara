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

(provide 'zara-conversation-context-test)
;;; zara-conversation-context-test.el ends here
