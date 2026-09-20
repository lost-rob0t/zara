(in-package :zara)

(defstruct (%promise (:constructor %make-promise ()))
  (lock (bt:make-lock "zara-promise"))
  (condition (bt:make-condition-variable))
  done-p
  value
  error)

(defun %promise-resolve (promise value)
  (bt:with-lock-held ((%promise-lock promise))
    (unless (%promise-done-p promise)
      (setf (%promise-value promise) value
            (%promise-done-p promise) t)
      (bt:condition-notify (%promise-condition promise))))
  value)

(defun %promise-reject (promise error)
  (bt:with-lock-held ((%promise-lock promise))
    (unless (%promise-done-p promise)
      (setf (%promise-error promise) error
            (%promise-done-p promise) t)
      (bt:condition-notify (%promise-condition promise))))
  error)

(defun %monotonic-seconds ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defun %promise-await (promise timeout operation)
  (let ((deadline (+ (%monotonic-seconds) timeout)))
    (bt:with-lock-held ((%promise-lock promise))
      (loop until (%promise-done-p promise)
            for remaining = (- deadline (%monotonic-seconds))
            do (when (<= remaining 0)
                 (error 'zara-timeout :operation operation))
               (bt:condition-wait (%promise-condition promise)
                                  (%promise-lock promise)
                                  :timeout remaining))
      (when (%promise-error promise)
        (error (%promise-error promise)))
      (%promise-value promise))))

(defstruct %outbound
  message
  promise
  kind
  deadline)

(defstruct %pending
  promise
  kind
  deadline)

(defstruct (client (:constructor %make-client))
  endpoint
  (state :new)
  state-lock
  session-id
  conversation-id
  thread
  (stop-p nil)
  outbound-lock
  outbound-queue
  outbound-limit
  event-lock
  event-condition
  event-queue
  event-limit
  pending
  request-timeout
  poll-interval-ms)

(defun make-client (&key endpoint
                         (request-timeout 5.0d0)
                         (outbound-limit 256)
                         (event-limit 256)
                         (poll-interval-ms 10))
  "Create a text/runtime ZARA/1 client. Initial CL support is owner-local IPC."
  (unless (and (stringp endpoint) (plusp (length endpoint)))
    (error "ENDPOINT must be a non-empty string"))
  (unless (uiop:string-prefix-p "ipc://" endpoint)
    (error "The Common Lisp Zara client currently supports authenticated owner-local IPC only"))
  (%make-client
   :endpoint endpoint
   :state-lock (bt:make-lock "zara-client-state")
   :outbound-lock (bt:make-lock "zara-client-outbound")
   :outbound-queue '()
   :outbound-limit outbound-limit
   :event-lock (bt:make-lock "zara-client-events")
   :event-condition (bt:make-condition-variable)
   :event-queue '()
   :event-limit event-limit
   :pending (make-hash-table :test #'equal)
   :request-timeout (coerce request-timeout 'double-float)
   :poll-interval-ms poll-interval-ms))

(defun %set-client-state (client state)
  (bt:with-lock-held ((client-state-lock client))
    (setf (client-state client) state)))

(defun %ensure-ready (client)
  (unless (eq (client-state client) :ready)
    (error 'client-not-ready :state (client-state client))))

(defun %enqueue-outbound (client outbound)
  (bt:with-lock-held ((client-outbound-lock client))
    (when (>= (length (client-outbound-queue client))
              (client-outbound-limit client))
      (error 'client-backpressure :limit (client-outbound-limit client)))
    (setf (client-outbound-queue client)
          (nconc (client-outbound-queue client) (list outbound)))))

(defun %drain-outbound (client)
  (bt:with-lock-held ((client-outbound-lock client))
    (prog1 (client-outbound-queue client)
      (setf (client-outbound-queue client) '()))))

(defun %enqueue-event (client message)
  (bt:with-lock-held ((client-event-lock client))
    (let ((queue (client-event-queue client)))
      (when (>= (length queue) (client-event-limit client))
        (setf queue (cdr queue)))
      (setf (client-event-queue client) (nconc queue (list message)))
      (bt:condition-notify (client-event-condition client)))))

(defun %send-frames (socket frames)
  (loop for index from 0 below (length frames)
        for more-p = (< index (1- (length frames)))
        do (pzmq:send socket (aref frames index) :sndmore more-p)))

(defun %recv-frames (socket)
  (multiple-value-bind (first more-p) (pzmq:recv-octets socket)
    (let ((frames (list first)))
      (loop while more-p
            do (multiple-value-bind (frame next-more-p)
                   (pzmq:recv-octets socket)
                 (push frame frames)
                 (setf more-p next-more-p)))
      (coerce (nreverse frames) 'vector))))

(defun %remote-error-from-message (message)
  (let ((body (protocol-message-body message)))
    (make-condition
     'remote-error
     :code (%json-value body "code" "protocol_error")
     :message (%json-value body "message" "remote protocol error")
     :retryable-p (not (null (%json-value body "retryable" nil))))))

(defun %resolve-pending (client message)
  (let* ((reply-to (protocol-message-reply-to message))
         (pending (and reply-to (gethash reply-to (client-pending client)))))
    (unless pending
      (return-from %resolve-pending nil))
    (remhash reply-to (client-pending client))
    (let ((promise (%pending-promise pending))
          (kind (%pending-kind pending)))
      (cond
        ((string= (protocol-message-type message) "protocol.error")
         (%promise-reject promise (%remote-error-from-message message)))
        ((eq kind :hello)
         (if (and (string= (protocol-message-type message) "hello.ok")
                  (protocol-message-session-id message))
             (progn
               (setf (client-session-id client)
                     (protocol-message-session-id message))
               (%set-client-state client :ready)
               (%promise-resolve promise message))
             (%promise-reject promise
                              (make-condition 'protocol-error
                                              :reason "invalid hello response"))))
        ((and (eq kind :ping) (string= (protocol-message-type message) "pong"))
         (%promise-resolve promise message))
        ((and (eq kind :status)
              (string= (protocol-message-type message) "runtime.status.ok"))
         (%promise-resolve promise message))
        ((and (eq kind :conversation)
              (string= (protocol-message-type message) "conversation.opened")
              (protocol-message-conversation-id message))
         (setf (client-conversation-id client)
               (protocol-message-conversation-id message))
         (%promise-resolve promise message))
        ((and (eq kind :command)
              (member (protocol-message-type message)
                      '("turn.accepted" "turn.cancel.accepted"
                        "tool.approve.accepted" "tool.reject.accepted")
                      :test #'string=))
         (%promise-resolve promise message))
        (t
         (%promise-reject promise
                          (make-condition 'protocol-error
                                          :reason (format nil
                                                          "unexpected ~a reply for ~a"
                                                          (protocol-message-type message)
                                                          kind))))))
    t))

(defun %expire-pending (client)
  (let ((now (%monotonic-seconds))
        (expired '()))
    (maphash (lambda (id pending)
               (when (<= (%pending-deadline pending) now)
                 (push (cons id pending) expired)))
             (client-pending client))
    (dolist (entry expired)
      (remhash (car entry) (client-pending client))
      (%promise-reject (%pending-promise (cdr entry))
                       (make-condition 'zara-timeout
                                       :operation (%pending-kind (cdr entry)))))))

(defun %fail-pending (client error)
  (maphash (lambda (_id pending)
             (declare (ignore _id))
             (%promise-reject (%pending-promise pending) error))
           (client-pending client))
  (clrhash (client-pending client)))

(defun %worker-send-outbound (client socket outbound)
  (let* ((message (%outbound-message outbound))
         (id (protocol-message-id message)))
    (setf (gethash id (client-pending client))
          (make-%pending :promise (%outbound-promise outbound)
                         :kind (%outbound-kind outbound)
                         :deadline (%outbound-deadline outbound)))
    (%send-frames socket (encode-message message))))

(defun %client-worker (client startup-promise)
  (handler-case
      (pzmq:with-context (context :io-threads 1)
        (pzmq:with-socket (socket context) (:dealer :linger 0 :sndhwm 256 :rcvhwm 256)
          (pzmq:connect socket (client-endpoint client))
          (let* ((hello-id (%message-id))
                 (hello (%make-protocol-message
                         :type "hello"
                         :id hello-id
                         :timestamp-ns (%now-nanoseconds)
                         :body (%json-object "versions" (vector 1)))))
            (setf (gethash hello-id (client-pending client))
                  (make-%pending :promise startup-promise
                                 :kind :hello
                                 :deadline (+ (%monotonic-seconds)
                                              (client-request-timeout client))))
            (%send-frames socket (encode-message hello)))
          (pzmq:with-poll-items items ((socket :pollin))
            (loop until (client-stop-p client)
                  do (dolist (outbound (%drain-outbound client))
                       (%worker-send-outbound client socket outbound))
                     (%expire-pending client)
                     (pzmq:poll items (client-poll-interval-ms client))
                     (when (member :pollin (pzmq:revents items 0))
                       (let ((message (decode-message (%recv-frames socket))))
                         (unless (%resolve-pending client message)
                           (%enqueue-event client message))))))))
    (error (condition)
      (unless (%promise-done-p startup-promise)
        (%promise-reject startup-promise condition))
      (%set-client-state client :failed)
      (%fail-pending client condition)))
  (unless (eq (client-state client) :failed)
    (%set-client-state client :stopped))
  (%fail-pending client
                 (make-condition 'client-not-ready :state (client-state client))))

(defun start-client (client &key (timeout (client-request-timeout client)))
  "Start CLIENT and complete the ZARA/1 hello handshake. Returns CLIENT."
  (check-type client client)
  (bt:with-lock-held ((client-state-lock client))
    (when (eq (client-state client) :ready)
      (return-from start-client client))
    (unless (member (client-state client) '(:new :stopped))
      (error 'client-not-ready :state (client-state client)))
    (setf (client-state client) :starting
          (client-stop-p client) nil))
  (let ((startup (%make-promise)))
    (setf (client-thread client)
          (bt:make-thread (lambda () (%client-worker client startup))
                          :name "zara-cl-client"))
    (handler-case
        (progn
          (%promise-await startup timeout "handshake")
          client)
      (error (condition)
        (setf (client-stop-p client) t)
        (%set-client-state client :failed)
        (error condition)))))

(defun close-client (client)
  "Stop CLIENT. Safe to call more than once."
  (check-type client client)
  (setf (client-stop-p client) t)
  (let ((thread (client-thread client)))
    (when thread
      (ignore-errors (bt:join-thread thread))
      (setf (client-thread client) nil)))
  (unless (eq (client-state client) :failed)
    (%set-client-state client :stopped))
  client)

(defmacro with-client ((name &rest options) &body body)
  `(let ((,name (make-client ,@options)))
     (unwind-protect
          (progn
            (start-client ,name)
            ,@body)
       (close-client ,name))))

(defun %request (client message kind &key (timeout (client-request-timeout client)))
  (%ensure-ready client)
  (let ((promise (%make-promise)))
    (%enqueue-outbound
     client
     (make-%outbound :message message
                     :promise promise
                     :kind kind
                     :deadline (+ (%monotonic-seconds) timeout)))
    (%promise-await promise timeout kind)))

(defun %base-message (client type &key id conversation-id turn-id body)
  (%make-protocol-message
   :type type
   :id (or id (%message-id))
   :session-id (client-session-id client)
   :conversation-id conversation-id
   :turn-id turn-id
   :timestamp-ns (%now-nanoseconds)
   :body body))

(defun ping (client &key (timeout (client-request-timeout client)))
  (%request client (%base-message client "ping") :ping :timeout timeout))

(defun runtime-status (client &key (timeout (client-request-timeout client)))
  (let ((reply (%request client (%base-message client "runtime.status")
                         :status :timeout timeout)))
    (values (message-body-value reply "state") reply)))

(defun open-conversation (client &optional conversation-id
                          &key (timeout (client-request-timeout client)))
  (let ((reply (%request
                client
                (%base-message client "conversation.open"
                               :conversation-id conversation-id)
                :conversation
                :timeout timeout)))
    (values (protocol-message-conversation-id reply) reply)))

(defun submit-turn (client text &key
                                  conversation-id
                                  context-ids
                                  request-id
                                  (timeout (client-request-timeout client)))
  (unless (and (stringp text)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           text))))
    (error "TEXT must be non-empty"))
  (let* ((conversation-id (or conversation-id (client-conversation-id client)))
         (body (%json-object "text" text
                             "context_ids" (coerce (or context-ids '()) 'vector)))
         (reply (%request
                 client
                 (%base-message client "turn.submit"
                                :id request-id
                                :conversation-id conversation-id
                                :body body)
                 :command
                 :timeout timeout)))
    (values (protocol-message-turn-id reply) reply)))

(defun cancel-turn (client turn-id &key request-id
                                        (timeout (client-request-timeout client)))
  (%request client
            (%base-message client "turn.cancel"
                           :id request-id
                           :turn-id turn-id)
            :command
            :timeout timeout))

(defun approve-tool (client tool-run-id &key request-id
                                             (timeout (client-request-timeout client)))
  (%request client
            (%base-message client "tool.approve"
                           :id request-id
                           :body (%json-object "tool_run_id" tool-run-id))
            :command
            :timeout timeout))

(defun reject-tool (client tool-run-id &key (reason "") request-id
                                            (timeout (client-request-timeout client)))
  (%request client
            (%base-message client "tool.reject"
                           :id request-id
                           :body (%json-object "tool_run_id" tool-run-id
                                               "reason" reason))
            :command
            :timeout timeout))

(defun next-event (client &key (timeout (client-request-timeout client)))
  "Return the next unsolicited runtime event received from Zara."
  (let ((deadline (+ (%monotonic-seconds) timeout)))
    (bt:with-lock-held ((client-event-lock client))
      (loop
        (when (client-event-queue client)
          (let ((event (car (client-event-queue client))))
            (setf (client-event-queue client) (cdr (client-event-queue client)))
            (return event)))
        (let ((remaining (- deadline (%monotonic-seconds))))
          (when (<= remaining 0)
            (error 'zara-timeout :operation "runtime event"))
          (bt:condition-wait (client-event-condition client)
                             (client-event-lock client)
                             :timeout remaining))))))

(defun ask (client text &key conversation-id context-ids
                              (timeout 60.0d0))
  "Submit TEXT and wait for the final assistant text for that turn.
Returns the text as the primary value and the final protocol message second."
  (unless (or conversation-id (client-conversation-id client))
    (setf conversation-id
          (open-conversation client nil :timeout (min timeout 5.0d0))))
  (let* ((conversation-id (or conversation-id (client-conversation-id client)))
         (deadline (+ (%monotonic-seconds) timeout))
         (turn-id (submit-turn client text
                               :conversation-id conversation-id
                               :context-ids context-ids
                               :timeout (min timeout
                                             (client-request-timeout client)))))
    (loop
      for remaining = (- deadline (%monotonic-seconds))
      do (when (<= remaining 0)
           (error 'zara-timeout :operation "assistant response"))
         (let ((event (next-event client :timeout remaining)))
           (when (and (equal turn-id (protocol-message-turn-id event))
                      (member (protocol-message-type event)
                              '("assistant.response" "assistant.completed")
                              :test #'string=))
             (return (values (message-body-value event "text" "") event)))))))
