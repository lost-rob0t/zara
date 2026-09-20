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
    (let ((json (com.inuoe.jzon:parse (aref frames 1))))
      (is (string= "hello" (gethash "type" json)))
      (is (string= "hello-1" (gethash "id" json)))
      (is (= 0 (gethash "payload_count" json))))))

(test decode-hello-ok
  (let* ((envelope (com.inuoe.jzon:stringify
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
         (json (com.inuoe.jzon:parse (aref frames 1))))
    (is (string= "turn.submit" (gethash "type" json)))
    (is (string= "session-1" (gethash "session_id" json)))
    (is (string= "conversation-1" (gethash "conversation_id" json)))
    (is (string= "hello" (gethash "text" (gethash "body" json))))))

(test unknown-server-type-fails-closed
  (let* ((envelope (com.inuoe.jzon:stringify
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

(test duplicate-envelope-key-fails-closed
  (let ((frames
          (vector
           (babel:string-to-octets "ZARA/1" :encoding :utf-8)
           (babel:string-to-octets
            "{\"type\":\"pong\",\"type\":\"hello.ok\",\"id\":\"x\",\"timestamp_ns\":1,\"payload_count\":0}"
            :encoding :utf-8))))
    (signals zara:protocol-error
      (zara:decode-message frames))))
