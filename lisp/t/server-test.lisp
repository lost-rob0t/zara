(in-package :zara/tests)

(test stop-server-bounds-child-shutdown
  (let ((process
          (uiop:launch-program
           '("sh" "-c" "trap '' TERM; sleep 2")
           :input :null
           :output :null
           :error-output :null
           :wait nil))
        (server nil))
    (unwind-protect
         (progn
           (setf server
                 (zara::%make-server
                  :process process
                  :endpoint "ipc:///tmp/zara-stop-test.sock"
                  :runtime-dir (uiop:temporary-directory)
                  :executable "sh"
                  :shutdown-timeout 0.05d0))
           (let ((started (get-internal-real-time)))
             (zara:stop-server server)
             (let ((elapsed
                     (/ (- (get-internal-real-time) started)
                        (float internal-time-units-per-second 1.0d0))))
               (is (< elapsed 0.75d0))))
           (is (not (ignore-errors (uiop:process-alive-p process)))))
      (when (ignore-errors (uiop:process-alive-p process))
        (ignore-errors (uiop:terminate-process process :urgent t)))
      (ignore-errors (uiop:wait-process process)))))

(test real-daemon-handshake
  (when (uiop:getenv "ZARA_LISP_INTEGRATION")
    (zara:with-server (server)
      (zara:with-client
          (client
           :endpoint (zara:server-endpoint server))
        (is (zara:ping client))
        (multiple-value-bind (state reply)
            (zara:runtime-status client)
          (declare (ignore reply))
          (is (member state
                      '("ready" "degraded")
                      :test #'string=)))
        (multiple-value-bind (conversation-id reply)
            (zara:open-conversation client)
          (declare (ignore reply))
          (is (stringp conversation-id))
          (is (plusp
               (length conversation-id))))))))
