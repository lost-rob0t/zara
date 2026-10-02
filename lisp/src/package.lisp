(defpackage :zara
  (:use :cl)
  (:export
   ;; Conditions.
   #:zara-error
   #:protocol-error
   #:remote-error
   #:remote-error-code
   #:remote-error-retryable-p
   #:zara-timeout
   #:client-not-ready
   #:client-backpressure
   ;; Protocol messages.
   #:protocol-message
   #:protocol-message-type
   #:protocol-message-id
   #:protocol-message-reply-to
   #:protocol-message-session-id
   #:protocol-message-conversation-id
   #:protocol-message-turn-id
   #:protocol-message-stream-id
   #:protocol-message-seq
   #:protocol-message-trace-id
   #:protocol-message-content-type
   #:protocol-message-payloads
   #:protocol-message-body
   #:message-body-value
   #:encode-message
   #:decode-message
   ;; Client lifecycle.
   #:client
   #:make-client
   #:client-endpoint
   #:client-state
   #:client-session-id
   #:client-conversation-id
   #:start-client
   #:close-client
   #:with-client
   ;; ZARA/1 operations.
   #:ping
   #:runtime-status
   #:open-conversation
   #:submit-turn
   #:cancel-turn
   #:approve-tool
   #:reject-tool
   #:next-event
   #:ask
   ;; Server lifecycle.
   #:server
   #:start-server
   #:stop-server
   #:server-endpoint
   #:server-runtime-dir
   #:server-alive-p
   #:with-server))
