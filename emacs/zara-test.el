;;; zara-test.el --- Tests for native Zara Emacs bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(load-file
 (expand-file-name
  "zara.el"
  (file-name-directory (or load-file-name buffer-file-name))))

(defun zara-test--request (operation &optional args)
  "Call bridge OPERATION with ARGS and decode the response."
  (let ((request (make-hash-table :test #'equal)))
    (puthash "bridge" zara-bridge-version request)
    (puthash "operation" operation request)
    (puthash "args" (or args (make-hash-table :test #'equal)) request)
    (json-parse-string
     (zara-bridge-call
      (base64-encode-string
       (encode-coding-string (json-serialize request) 'utf-8-unix)
       t))
     :object-type 'hash-table
     :array-type 'list
     :null-object nil
     :false-object :false)))

(defun zara-test--args (&rest pairs)
  "Return request args hash table from PAIRS."
  (let ((args (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) args))
    args))

(ert-deftest zara-bridge-session-describe-is-versioned ()
  (let* ((response (zara-test--request "session.describe"))
         (result (gethash "result" response)))
    (should (eq (gethash "ok" response) t))
    (should (equal (gethash "bridge" response) "ZARA-EMACS/1"))
    (should (equal (gethash "bridge" result) "ZARA-EMACS/1"))
    (should (member "edit.preview" (gethash "capabilities" result)))))

(ert-deftest zara-bridge-rejects-unknown-operation ()
  (let* ((response (zara-test--request "elisp.eval"))
         (error-object (gethash "error" response)))
    (should (eq (gethash "ok" response) :false))
    (should (string-match-p
             "unknown bridge operation"
             (gethash "message" error-object)))))

(ert-deftest zara-bridge-buffer-context-and-read-use-opaque-id ()
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 7)
    (let* ((context-response (zara-test--request "buffer.context"))
           (context (gethash "result" context-response))
           (buffer-id (gethash "buffer_id" context))
           (read-response
            (zara-test--request
             "buffer.read"
             (zara-test--args
              "buffer_id" buffer-id
              "start" 1
              "end" 6)))
           (read-result (gethash "result" read-response)))
      (should (string-prefix-p "b-" buffer-id))
      (should (= (gethash "point" context) 7))
      (should (equal (gethash "text" read-result) "alpha")))))

(ert-deftest zara-bridge-edit-preview-apply-is-revision-safe-and-undoable ()
  (with-temp-buffer
    (insert "alpha beta")
    (buffer-enable-undo)
    (undo-boundary)
    (let* ((context
            (gethash "result" (zara-test--request "buffer.context")))
           (buffer-id (gethash "buffer_id" context))
           (tick (gethash "modified_tick" context))
           (preview
            (gethash
             "result"
             (zara-test--request
              "edit.preview"
              (zara-test--args
               "buffer_id" buffer-id
               "start" 7
               "end" 11
               "expected_tick" tick
               "replacement" "BETA"))))
           (edit-id (gethash "edit_id" preview))
           (apply-response
            (zara-test--request
             "edit.apply"
             (zara-test--args "edit_id" edit-id))))
      (should (eq (gethash "ok" apply-response) t))
      (should (equal (buffer-string) "alpha BETA"))
      (undo)
      (should (equal (buffer-string) "alpha beta")))))

(ert-deftest zara-bridge-edit-apply-rejects-stale-buffer ()
  (with-temp-buffer
    (insert "abc")
    (let* ((context
            (gethash "result" (zara-test--request "buffer.context")))
           (preview
            (gethash
             "result"
             (zara-test--request
              "edit.preview"
              (zara-test--args
               "buffer_id" (gethash "buffer_id" context)
               "start" 1
               "end" 2
               "expected_tick" (gethash "modified_tick" context)
               "replacement" "A"))))
           (edit-id (gethash "edit_id" preview)))
      (goto-char (point-max))
      (insert "!")
      (let* ((response
              (zara-test--request
               "edit.apply"
               (zara-test--args "edit_id" edit-id)))
             (error-object (gethash "error" response)))
        (should (eq (gethash "ok" response) :false))
        (should (string-match-p
                 "stale buffer revision"
                 (gethash "message" error-object)))))))

(ert-deftest zara-bridge-command-invoke-is-closed-and-extensible ()
  (let ((missing
         (zara-test--request
          "command.invoke"
          (zara-test--args "adapter" "not-registered"))))
    (should (eq (gethash "ok" missing) :false)))
  (zara-register-command-adapter
   "test.echo"
   (lambda (args)
     `((value . ,(gethash "value" args)))))
  (let* ((response
          (zara-test--request
           "command.invoke"
           (zara-test--args
            "adapter" "test.echo"
            "value" "ok")))
         (result (gethash "result" response)))
    (should (equal (gethash "value" result) "ok"))))

(provide 'zara-test)
;;; zara-test.el ends here
