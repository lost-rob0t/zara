(in-package :zara)

(defun %request (client
                 message
                 kind
                 &key
                   (timeout
                     (client-request-timeout client)))
  (%ensure-ready client)
  (let ((promise (%make-promise)))
    (%enqueue-outbound
     client
     (make-%outbound
      :message message
      :promise promise
      :kind kind
      :deadline (+ (%monotonic-seconds) timeout)))
    (%promise-await promise timeout kind)))

(defun %base-message (client
                      type
                      &key
                        id
                        conversation-id
                        turn-id
                        body)
  (%make-protocol-message
   :type type
   :id (or id (%message-id))
   :session-id (client-session-id client)
   :conversation-id conversation-id
   :turn-id turn-id
   :timestamp-ns (%now-nanoseconds)
   :body body))

(defun ping (client
             &key
               (timeout
                 (client-request-timeout client)))
  (%request client
            (%base-message client "ping")
            :ping
            :timeout timeout))

(defun runtime-status (client
                       &key
                         (timeout
                           (client-request-timeout client)))
  (let ((reply
          (%request client
                    (%base-message
                     client
                     "runtime.status")
                    :status
                    :timeout timeout)))
    (values (message-body-value reply "state")
            reply)))

(defun open-conversation (client
                          &optional conversation-id
                          &key
                            (timeout
                              (client-request-timeout client)))
  (let ((reply
          (%request
           client
           (%base-message
            client
            "conversation.open"
            :conversation-id conversation-id)
           :conversation
           :timeout timeout)))
    (values (protocol-message-conversation-id reply)
            reply)))

(defun submit-turn (client
                    text
                    &key
                      conversation-id
                      context-ids
                      request-id
                      (timeout
                        (client-request-timeout client)))
  (unless
      (and (stringp text)
           (plusp
            (length
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              text))))
    (error "TEXT must be non-empty"))
  (let* ((conversation-id
           (or conversation-id
               (client-conversation-id client)))
         (body
           (%json-object
            "text" text
            "context_ids"
            (coerce (or context-ids '()) 'vector)))
         (reply
           (%request
            client
            (%base-message
             client
             "turn.submit"
             :id request-id
             :conversation-id conversation-id
             :body body)
            :command
            :timeout timeout)))
    (values (protocol-message-turn-id reply)
            reply)))

(defun cancel-turn (client
                    turn-id
                    &key
                      request-id
                      (timeout
                        (client-request-timeout client)))
  (%request
   client
   (%base-message client
                  "turn.cancel"
                  :id request-id
                  :turn-id turn-id)
   :command
   :timeout timeout))

(defun approve-tool (client
                     tool-run-id
                     &key
                       request-id
                       (timeout
                         (client-request-timeout client)))
  (%request
   client
   (%base-message
    client
    "tool.approve"
    :id request-id
    :body (%json-object
           "tool_run_id"
           tool-run-id))
   :command
   :timeout timeout))

(defun reject-tool (client
                    tool-run-id
                    &key
                      (reason "")
                      request-id
                      (timeout
                        (client-request-timeout client)))
  (%request
   client
   (%base-message
    client
    "tool.reject"
    :id request-id
    :body (%json-object
           "tool_run_id" tool-run-id
           "reason" reason))
   :command
   :timeout timeout))

(defun next-event (client
                   &key
                     (timeout
                       (client-request-timeout client)))
  "Return the next unsolicited runtime event received from Zara."
  (let ((deadline
          (+ (%monotonic-seconds) timeout)))
    (bt:with-lock-held ((client-event-lock client))
      (loop
        (when (client-event-queue client)
          (let ((event
                  (car
                   (client-event-queue client))))
            (setf (client-event-queue client)
                  (cdr
                   (client-event-queue client)))
            (return event)))
        (when (%stop-requested-p client)
          (error
           'client-not-ready
           :state (%client-state-value client)))
        (let ((remaining
                (- deadline
                   (%monotonic-seconds))))
          (when (<= remaining 0)
            (error 'zara-timeout
                   :operation "runtime event"))
          (bt:condition-wait
           (client-event-condition client)
           (client-event-lock client)
           :timeout remaining))))))

(defun ask (client
            text
            &key
              conversation-id
              context-ids
              (timeout 60.0d0))
  "Submit TEXT and wait for the final assistant text for that turn.

This serial convenience API consumes runtime events. Concurrent callers should
use SUBMIT-TURN and NEXT-EVENT explicitly."
  (unless (or conversation-id
              (client-conversation-id client))
    (setf conversation-id
          (open-conversation
           client
           nil
           :timeout (min timeout 5.0d0))))
  (let* ((conversation-id
           (or conversation-id
               (client-conversation-id client)))
         (deadline
           (+ (%monotonic-seconds) timeout))
         (turn-id
           (submit-turn
            client
            text
            :conversation-id conversation-id
            :context-ids context-ids
            :timeout
            (min timeout
                 (client-request-timeout client)))))
    (loop
      for remaining =
        (- deadline (%monotonic-seconds))
      do (when (<= remaining 0)
           (error 'zara-timeout
                  :operation "assistant response"))
         (let ((event
                 (next-event client
                             :timeout remaining)))
           (when
               (and
                (equal turn-id
                       (protocol-message-turn-id
                        event))
                (member
                 (protocol-message-type event)
                 '("assistant.response"
                   "assistant.completed")
                 :test #'string=))
             (return
               (values
                (message-body-value
                 event
                 "text"
                 "")
                event)))))))
