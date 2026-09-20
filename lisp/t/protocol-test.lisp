(in-package :zara/tests)

(test client-hello-shape
  (let* ((message (zara::%make-protocol-message
                   :type "hello"
                   :id "hello-1"
                   :timestamp-ns 1
                   :body (zara::%json-object "versions" (vector 1))))
         (frames (zara:encode-message message)))
    (is (= 2 (length frames)))
    (is (equalp (babel:string-to-octets "ZARA/1" :encoding :utf-8)
                (aref frames 0)))
    (let ((json (jsown:parse (babel:octets-to-string (aref frames 1)
                                                     :encoding :utf-8))))
      (is (string= "hello" (jsown:val json "type")))
      (is (string= "hello-1" (jsown:val json "id")))
      (is (= 0 (jsown:val json "payload_count"))))))

(test decode-hello-ok
  (let* ((envelope (jsown:to-json
                    (zara::%json-object
                     "type" "hello.ok"
                     "id" "server-1"
                     "reply_to" "hello-1"
                     "session_id" "session-1"
                     "timestamp_ns" 2
                     "payload_count" 0
                     "body" (zara::%json-object "version" 1))))
         (frames (vector
                  (babel:string-to-octets "ZARA/1" :encoding :utf-8)
                  (babel:string-to-octets envelope :encoding :utf-8)))
         (message (zara:decode-message frames)))
    (is (string= "hello.ok" (zara:protocol-message-type message)))
    (is (string= "hello-1" (zara:protocol-message-reply-to message)))
    (is (string= "session-1" (zara:protocol-message-session-id message)))))

(test turn-submit-is-closed-zara1
  (let* ((message (zara::%make-protocol-message
                   :type "turn.submit"
                   :id "request-1"
                   :session-id "session-1"
                   :conversation-id "conversation-1"
                   :timestamp-ns 3
                   :body (zara::%json-object
                          "text" "hello"
                          "context_ids" (vector "ctx-a"))))
         (frames (zara:encode-message message))
         (json (jsown:parse (babel:octets-to-string (aref frames 1)
                                                    :encoding :utf-8))))
    (is (string= "turn.submit" (jsown:val json "type")))
    (is (string= "session-1" (jsown:val json "session_id")))
    (is (string= "conversation-1" (jsown:val json "conversation_id")))
    (is (string= "hello" (jsown:val (jsown:val json "body") "text")))))

(test unknown-server-type-fails-closed
  (let* ((envelope (jsown:to-json
                    (zara::%json-object
                     "type" "future.magic"
                     "id" "server-1"
                     "timestamp_ns" 2
                     "payload_count" 0)))
         (frames (vector
                  (babel:string-to-octets "ZARA/1" :encoding :utf-8)
                  (babel:string-to-octets envelope :encoding :utf-8))))
    (signals zara:protocol-error
      (zara:decode-message frames))))
