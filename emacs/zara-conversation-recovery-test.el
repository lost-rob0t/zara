;;; zara-conversation-recovery-test.el --- Recovery ERT for Zara conversation -*- lexical-binding: t; -*-

(require 'ert)
(load-file
 (expand-file-name
  "zara-conversation-test.el"
  (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest zara-conversation-cancel-without-turn-receipt-clears-busy-state ()
  (let ((target (generate-new-buffer " *zara-conversation-target*"))
        response
        error
        process)
    (unwind-protect
        (progn
          (setq process
                (zara-conversation-test--process
                 target
                 (lambda (value failure)
                   (setq response value
                         error failure))))
          (with-current-buffer target
            (cl-incf zara-conversation--generation)
            (setq zara-conversation--state 'cancelling))
          (process-put process 'zara-cancel-requested t)
          ;; Simulate the native request exiting before turn.accepted.  No
          ;; CancelTurn can be targeted because Zara never minted a turn id.
          (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_process) 2)))
            (zara-conversation--process-sentinel process "finished"))
          (should-not response)
          (should (string-match-p "before cancellation received a turn id" error))
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'error))
            (should-not zara-conversation--request-process)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(provide 'zara-conversation-recovery-test)
;;; zara-conversation-recovery-test.el ends here
