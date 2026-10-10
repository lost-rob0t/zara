(in-package :zara)

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
         (pending
           (and reply-to
                (gethash reply-to (client-pending client)))))
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
               (%mark-ready client (protocol-message-session-id message))
               (%promise-resolve promise message))
             (%promise-reject
              promise
              (make-condition 'protocol-error
                              :reason "invalid hello response"))))
        ((and (eq kind :ping)
              (string= (protocol-message-type message) "pong"))
         (%promise-resolve promise message))
        ((and (eq kind :status)
              (string= (protocol-message-type message)
                       "runtime.status.ok"))
         (%promise-resolve promise message))
        ((and (eq kind :conversation)
              (string= (protocol-message-type message)
                       "conversation.opened")
              (protocol-message-conversation-id message))
         (bt:with-lock-held ((client-state-lock client))
           (setf (client-conversation-id client)
                 (protocol-message-conversation-id message)))
         (%promise-resolve promise message))
        ((and (eq kind :command)
              (member (protocol-message-type message)
                      '("turn.accepted"
                        "turn.cancel.accepted"
                        "tool.approve.accepted"
                        "tool.reject.accepted")
                      :test #'string=))
         (%promise-resolve promise message))
        (t
         (%promise-reject
          promise
          (make-condition
           'protocol-error
           :reason
           (format nil
                   "unexpected ~a reply for ~a"
                   (protocol-message-type message)
                   kind))))))
    t))

(defun %expire-pending (client)
  (let ((now (%monotonic-seconds))
        (expired '()))
    (maphash
     (lambda (id pending)
       (when (<= (%pending-deadline pending) now)
         (push (cons id pending) expired)))
     (client-pending client))
    (dolist (entry expired)
      (remhash (car entry) (client-pending client))
      (%promise-reject
       (%pending-promise (cdr entry))
       (make-condition 'zara-timeout
                       :operation (%pending-kind (cdr entry)))))))

(defun %fail-pending (client condition)
  (maphash
   (lambda (_id pending)
     (declare (ignore _id))
     (%promise-reject (%pending-promise pending) condition))
   (client-pending client))
  (clrhash (client-pending client)))

(defun %worker-send-outbound (client socket outbound)
  (let* ((message (%outbound-message outbound))
         (id (protocol-message-id message))
         (promise (%outbound-promise outbound)))
    (cond
      ((%promise-complete-p promise)
       nil)
      ((<= (%outbound-deadline outbound)
           (%monotonic-seconds))
       (%promise-reject
        promise
        (make-condition 'zara-timeout
                        :operation (%outbound-kind outbound))))
      ((gethash id (client-pending client))
       (%promise-reject
        promise
        (make-condition
         'protocol-error
         :reason (format nil
                         "request id ~a is already pending"
                         id))))
      (t
       (setf (gethash id (client-pending client))
             (make-%pending
              :promise promise
              :kind (%outbound-kind outbound)
              :deadline (%outbound-deadline outbound)))
       (%send-frames socket (encode-message message))))))

(defun %client-worker (client startup-promise)
  (handler-case
      (pzmq:with-context (context :io-threads 1)
        (pzmq:with-socket (socket context)
            (:dealer :linger 0 :sndhwm 256 :rcvhwm 256)
          (pzmq:connect socket (client-endpoint client))
          (let* ((hello-id (%message-id))
                 (hello
                   (%make-protocol-message
                    :type "hello"
                    :id hello-id
                    :timestamp-ns (%now-nanoseconds)
                    :body (%json-object
                           "versions"
                           (vector 1)))))
            (setf (gethash hello-id (client-pending client))
                  (make-%pending
                   :promise startup-promise
                   :kind :hello
                   :deadline
                   (+ (%monotonic-seconds)
                      (client-request-timeout client))))
            (%send-frames socket (encode-message hello)))
          (pzmq:with-poll-items items ((socket :pollin))
            (loop until (%stop-requested-p client)
                  do (dolist (outbound (%drain-outbound client))
                       (%worker-send-outbound
                        client
                        socket
                        outbound))
                     (%expire-pending client)
                     (pzmq:poll items
                                (client-poll-interval-ms client))
                     (when (member :pollin
                                   (pzmq:revents items 0))
                       (let ((message
                               (decode-message
                                (%recv-frames socket))))
                         (unless (%resolve-pending client message)
                           (%enqueue-event client message))))))))
    (error (condition)
      (unless (%promise-complete-p startup-promise)
        (%promise-reject startup-promise condition))
      (%set-client-state client :failed)
      (%fail-pending client condition)))
  (unless (eq (%client-state-value client) :failed)
    (%set-client-state client :stopped))
  (%fail-pending
   client
   (make-condition
    'client-not-ready
    :state (%client-state-value client)))
  (%wake-event-waiter client))

(defun start-client (client
                     &key
                       (timeout
                         (client-request-timeout client)))
  "Start CLIENT and complete the ZARA/1 hello handshake."
  (check-type client client)
  (when (eq (%prepare-client-start client) :already-ready)
    (return-from start-client client))
  (let ((startup (%make-promise)))
    (setf (client-thread client)
          (bt:make-thread
           (lambda ()
             (%client-worker client startup))
           :name "zara-cl-client"))
    (handler-case
        (progn
          (%promise-await startup timeout "handshake")
          client)
      (error (condition)
        (%request-stop client)
        (let ((thread (client-thread client)))
          (when thread
            (ignore-errors
              (bt:join-thread thread))
            (setf (client-thread client) nil)))
        (%set-client-state client :failed)
        (error condition)))))

(defun close-client (client)
  "Stop CLIENT. Safe to call more than once."
  (check-type client client)
  (%request-stop client)
  (%wake-event-waiter client)
  (let ((thread (client-thread client)))
    (when thread
      (ignore-errors
        (bt:join-thread thread))
      (setf (client-thread client) nil)))
  (unless (eq (%client-state-value client) :failed)
    (%set-client-state client :stopped))
  client)

(defmacro with-client ((name &rest options) &body body)
  (list 'let
        (list (list name
                    (cons 'make-client options)))
        (list 'unwind-protect
              (cons 'progn
                    (cons (list 'start-client name)
                          body))
              (list 'close-client name))))
