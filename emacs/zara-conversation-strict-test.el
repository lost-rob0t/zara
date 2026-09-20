;;; zara-conversation-strict-test.el --- Strict ERT for Zara conversation -*- lexical-binding: t; -*-

(require 'ert)
(load-file
 (expand-file-name
  "zara-conversation-recovery-test.el"
  (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest zara-conversation-duplicate-turn-accepted-fails-closed ()
  (let ((target (generate-new-buffer " *zara-conversation-target*"))
        error
        process)
    (unwind-protect
        (progn
          (setq process
                (zara-conversation-test--process
                 target
                 (lambda (_value failure)
                   (setq error failure))))
          (zara-conversation--handle-line process (zara-conversation-test--accepted))
          (should (equal (process-get process 'zara-turn-id) "turn-1"))
          (zara-conversation--handle-line process (zara-conversation-test--accepted))
          (should (string-match-p "duplicate turn.accepted" error))
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'error))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-busy-send-does-not-append-user-message ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-chat--busy t)
    (let ((before (buffer-string)))
      (should-error
       (zara-conversation-chat-send "duplicate request")
       :type 'user-error)
      (should (equal (buffer-string) before)))))

(provide 'zara-conversation-strict-test)
;;; zara-conversation-strict-test.el ends here
