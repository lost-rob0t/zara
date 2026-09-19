;;; zara-test.el --- Tests for Zara Emacs client -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'zara)

(ert-deftest zara-arguments-use-configured-daemon-endpoint ()
  (let ((zara-connect-endpoint "tcp://127.0.0.1:7731"))
    (should (equal (zara--arguments "hello")
                   '("--connect" "tcp://127.0.0.1:7731" "hello")))))

(ert-deftest zara-arguments-use-zara-default-endpoint-when-unset ()
  (let ((zara-connect-endpoint nil))
    (should (equal (zara--arguments "hello") '("hello")))))

(ert-deftest zara-arguments-reject-empty-prompts ()
  (should-error (zara--arguments "   ") :type 'user-error))

(ert-deftest zara-ask-runs-the-configured-command ()
  (let ((zara-program "printf")
        (zara-connect-endpoint nil))
    (should (equal (zara-ask "hello") "hello"))))

(ert-deftest zara-status-reports-program-and-endpoint ()
  (let ((zara-program "zara-test")
        (zara-connect-endpoint "ipc:///tmp/zara.sock"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/nix/store/test/bin/zara-test")))
      (should
       (equal
        (zara-status)
        '(:program "zara-test"
          :executable "/nix/store/test/bin/zara-test"
          :available t
          :endpoint "ipc:///tmp/zara.sock"))))))

(ert-deftest zara-chat-mode-exposes-send-binding ()
  (with-temp-buffer
    (zara-chat-mode)
    (should (eq (key-binding (kbd "s")) #'zara-chat-send))
    (should (eq (key-binding (kbd "RET")) #'zara-chat-send))))

(provide 'zara-test)
;;; zara-test.el ends here
