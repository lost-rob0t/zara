;;; zara-conversation-test.el --- ERT for Zara conversation control -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load-file
 (expand-file-name
  "zara.el"
  (file-name-directory (or load-file-name buffer-file-name))))
(load-file
 (expand-file-name
  "zara-conversation.el"
  (file-name-directory (or load-file-name buffer-file-name))))

(defun zara-conversation-test--process (target callback &optional generation)
  "Create inert request process owned by TARGET with CALLBACK and GENERATION."
  (let* ((generation (or generation 1))
         (process
          (make-pipe-process
           :name (generate-new-buffer-name "zara-conversation-test")
           :buffer nil
           :noquery t)))
    (process-put process 'zara-target target)
    (process-put process 'zara-generation generation)
    (process-put process 'zara-conversation-id "emacs-main")
    (process-put process 'zara-callback callback)
    (process-put process 'zara-line-buffer "")
    (with-current-buffer target
      (setq-local zara-conversation--generation generation)
      (setq-local zara-conversation--request-process process)
      (setq-local zara-conversation--state 'running)
      (setq-local zara-chat--busy t))
    process))

(defun zara-conversation-test--accepted (&optional conversation turn-id)
  "Return a turn.accepted event for CONVERSATION and TURN-ID."
  (json-serialize
   `((type . "turn.accepted")
     (conversation_id . ,(or conversation "emacs-main"))
     (turn_id . ,(or turn-id "turn-1")))))

(defun zara-conversation-test--complete (&optional conversation turn-id text)
  "Return assistant.complete event for CONVERSATION, TURN-ID and TEXT."
  (json-serialize
   `((type . "assistant.complete")
     (conversation_id . ,(or conversation "emacs-main"))
     (turn_id . ,(or turn-id "turn-1"))
     (text . ,(or text "hello")))))

(ert-deftest zara-conversation-turn-arguments-use-canonical-native-contract ()
  (let ((zara-connect-endpoint "ipc:///tmp/zara.sock"))
    (should
     (equal
      (zara-conversation--turn-arguments "emacs-main" "continue")
      '("--connect" "ipc:///tmp/zara.sock"
        "--conversation-id" "emacs-main"
        "--json-events" "continue")))
    (should
     (equal
      (zara-conversation--cancel-arguments "turn-9")
      '("--connect" "ipc:///tmp/zara.sock" "--cancel-turn" "turn-9")))))

(ert-deftest zara-conversation-default-id-is-stable-and-buffer-local ()
  (let ((zara-conversation-default-id "emacs-main"))
    (with-temp-buffer
      (should (equal (zara-conversation--current-id) "emacs-main"))
      (should (equal zara-conversation-id "emacs-main"))
      (should (equal (zara-conversation--current-id) "emacs-main")))))

(ert-deftest zara-conversation-accepted-complete-delivers-once ()
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
          (zara-conversation--handle-line process (zara-conversation-test--accepted))
          (should (equal (process-get process 'zara-turn-id) "turn-1"))
          (zara-conversation--handle-line
           process (zara-conversation-test--complete nil nil "symbolic reply"))
          (should (equal response "symbolic reply"))
          (should-not error)
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'complete))
            (should-not zara-conversation--request-process)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-mismatched-conversation-fails-closed ()
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
          (zara-conversation--handle-line
           process (zara-conversation-test--accepted "other-conversation"))
          (should-not response)
          (should (string-match-p "conversation_id mismatch" error))
          (should (process-get process 'zara-cancel-requested))
          (with-current-buffer target
            (should zara-chat--busy)
            (should (eq zara-conversation--state 'cancelling)))
          (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_process) 2)))
            (zara-conversation--process-sentinel process "finished"))
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'error))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-cancel-before-acceptance-uses-runtime-turn-id ()
  (let ((target (generate-new-buffer " *zara-conversation-target*"))
        response
        error
        cancelled-turn
        process)
    (unwind-protect
        (progn
          (setq process
                (zara-conversation-test--process
                 target
                 (lambda (value failure)
                   (setq response value
                         error failure))))
          (cl-letf (((symbol-function 'zara-conversation--start-cancel)
                     (lambda (request turn-id)
                       (setq cancelled-turn turn-id)
                       (process-put request 'zara-cancel-confirmed t)
                       (zara-conversation--finish-buffer request 'cancelled)
                       request)))
            (with-current-buffer target
              (zara-conversation-cancel)
              (should (eq zara-conversation--state 'cancelling)))
            (should-not cancelled-turn)
            (zara-conversation--handle-line
             process (zara-conversation-test--accepted nil "runtime-turn"))
            (should (equal cancelled-turn "runtime-turn"))
            ;; A late completion from the cancelled generation is parsed but
            ;; cannot reach the presentation callback.
            (zara-conversation--handle-line
             process
             (zara-conversation-test--complete nil "runtime-turn" "too late")))
          (should-not response)
          (should-not error)
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'cancelled))
            (should-not zara-conversation--request-process)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-old-generation-cannot-clobber-new-request ()
  (let ((target (generate-new-buffer " *zara-conversation-target*"))
        old-response
        old-error
        old-process
        new-process)
    (unwind-protect
        (progn
          (setq old-process
                (zara-conversation-test--process
                 target
                 (lambda (value failure)
                   (setq old-response value
                         old-error failure))
                 1))
          (process-put old-process 'zara-turn-id "old-turn")
          (setq new-process
                (zara-conversation-test--process target #'ignore 2))
          (zara-conversation--handle-line
           old-process
           (zara-conversation-test--complete nil "old-turn" "stale reply"))
          (should-not old-response)
          (should-not old-error)
          (with-current-buffer target
            (should zara-chat--busy)
            (should (eq zara-conversation--state 'running))
            (should (eq zara-conversation--request-process new-process))))
      (dolist (process (list old-process new-process))
        (when (and process (process-live-p process))
          (delete-process process)))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest zara-conversation-unknown-event-fails-closed ()
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
          (zara-conversation--handle-line
           process
           (json-serialize
            '((type . "provider.fallback")
              (conversation_id . "emacs-main")
              (turn_id . "turn-1"))))
          (should (string-match-p "unknown native-client event type" error))
          (should (process-get process 'zara-cancel-requested))
          (with-current-buffer target
            (should zara-chat--busy)
            (should (eq zara-conversation--state 'cancelling)))
          (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_process) 2)))
            (zara-conversation--process-sentinel process "finished"))
          (with-current-buffer target
            (should-not zara-chat--busy)
            (should (eq zara-conversation--state 'error))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(provide 'zara-conversation-test)
;;; zara-conversation-test.el ends here
