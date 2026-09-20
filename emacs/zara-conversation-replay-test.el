;;; zara-conversation-replay-test.el --- Replay contract tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'zara)
(require 'zara-conversation)

(defconst zara-conversation-replay-test--payload
  (concat
   "{\"conversation\":{\"created_at\":\"2026-09-20T18:00:00\","
   "\"id\":\"emacs-main\",\"title\":\"Emacs main\","
   "\"updated_at\":\"2026-09-20T18:05:00\"},"
   "\"messages\":["
   "{\"content\":\"hello\",\"error\":\"\",\"role\":\"user\","
   "\"sequence\":1,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-1\"},"
   "{\"content\":\"hi there\",\"error\":\"\",\"role\":\"assistant\","
   "\"sequence\":2,\"status\":\"complete\",\"tool_run_id\":null,"
   "\"turn_id\":\"turn-1\"}],"
   "\"version\":\"ZARA-CONVERSATION-REPLAY/1\"}"))

(ert-deftest zara-conversation-replay-is-local-canonical-read ()
  (let ((zara-connect-endpoint "tcp://should-not-be-used:9999"))
    (should
     (equal (zara-conversation--replay-arguments " emacs-main ")
            '("--replay-conversation" "emacs-main")))))

(ert-deftest zara-conversation-replay-rejects-wrong-conversation ()
  (should-error
   (zara-conversation--parse-replay
    zara-conversation-replay-test--payload
    "other")
   :type 'error))

(ert-deftest zara-conversation-replay-rejects-unknown-version ()
  (let ((payload
         (replace-regexp-in-string
          "ZARA-CONVERSATION-REPLAY/1"
          "ZARA-CONVERSATION-REPLAY/999"
          zara-conversation-replay-test--payload
          t t)))
    (should-error
     (zara-conversation--parse-replay payload "emacs-main")
     :type 'error)))

(ert-deftest zara-conversation-replay-renders-canonical-history ()
  (with-temp-buffer
    (zara-chat-mode)
    (setq-local zara-conversation-id "emacs-main")
    (let (captured)
      (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
                ((symbol-function 'process-file)
                 (lambda (program _infile _destination _display &rest args)
                   (setq captured (cons program args))
                   (insert zara-conversation-replay-test--payload)
                   0)))
        (zara-conversation-replay))
      (should
       (equal captured '("zara" "--replay-conversation" "emacs-main")))
      (should (string-match-p "Canonical conversation emacs-main" (buffer-string)))
      (should
       (string-match-p (regexp-quote "You\nhello") (buffer-string)))
      (should
       (string-match-p (regexp-quote "Zara\nhi there") (buffer-string))))))

(ert-deftest zara-conversation-replay-does-not-clobber-buffer-on-cli-error ()
  (with-temp-buffer
    (zara-chat-mode)
    (let ((inhibit-read-only t))
      (insert "keep-me"))
    (setq-local zara-conversation-id "emacs-main")
    (cl-letf (((symbol-function 'zara--program) (lambda () "zara"))
              ((symbol-function 'process-file)
               (lambda (&rest _args)
                 (insert "broken")
                 2)))
      (should-error (zara-conversation-replay) :type 'user-error))
    (should (string-match-p "keep-me" (buffer-string)))))

(provide 'zara-conversation-replay-test)
;;; zara-conversation-replay-test.el ends here
