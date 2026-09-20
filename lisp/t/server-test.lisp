(in-package :zara/tests)

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
