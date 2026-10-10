(in-package :zara/tests)

(test close-wakes-event-waiter
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-never-connect.sock"))
         (started (zara::%make-promise))
         (result nil)
         (thread
           (bt:make-thread
            (lambda ()
              (zara::%promise-resolve started t)
              (setf result
                    (handler-case
                        (progn
                          (zara:next-event client :timeout 1.0d0)
                          :unexpected-event)
                      (zara:client-not-ready ()
                        :closed)
                      (error (condition)
                        condition))))
            :name "zara-test-event-waiter")))
    (zara::%promise-await started 1.0d0 "test waiter")
    (zara:close-client client)
    (bt:join-thread thread)
    (is (eq :closed result))))

(test expired-outbound-never-enters-pending
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-expired.sock"))
         (promise (zara::%make-promise))
         (message
           (zara::%make-protocol-message
            :type "ping"
            :id "expired-request"
            :timestamp-ns 1))
         (outbound
           (zara::make-%outbound
            :message message
            :promise promise
            :kind :ping
            :deadline (- (zara::%monotonic-seconds) 1.0d0))))
    (zara::%worker-send-outbound client nil outbound)
    (is (null (gethash "expired-request"
                       (zara::client-pending client))))
    (signals zara:zara-timeout
      (zara::%promise-await promise 0.1d0 "expired outbound"))))

(test duplicate-pending-request-id-is-rejected
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-duplicate.sock"))
         (first-promise (zara::%make-promise))
         (second-promise (zara::%make-promise))
         (message
           (zara::%make-protocol-message
            :type "ping"
            :id "duplicate-request"
            :timestamp-ns 1))
         (outbound
           (zara::make-%outbound
            :message message
            :promise second-promise
            :kind :ping
            :deadline (+ (zara::%monotonic-seconds) 1.0d0))))
    (setf (gethash "duplicate-request"
                   (zara::client-pending client))
          (zara::make-%pending
           :promise first-promise
           :kind :ping
           :deadline (+ (zara::%monotonic-seconds) 1.0d0)))
    (zara::%worker-send-outbound client nil outbound)
    (signals zara:protocol-error
      (zara::%promise-await second-promise 0.1d0 "duplicate request"))
    (is (eq first-promise
            (zara::%pending-promise
             (gethash "duplicate-request"
                      (zara::client-pending client)))))))

(test owner-local-endpoint-is-enforced
  (signals error
    (zara:make-client :endpoint "tcp://127.0.0.1:5555")))

(test outbound-queue-limit-fails-closed
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-outbound-limit.sock"
            :outbound-limit 1))
         (first
           (zara::make-%outbound
            :message
            (zara::%make-protocol-message
             :type "ping"
             :id "queued-1"
             :timestamp-ns 1)
            :promise (zara::%make-promise)
            :kind :ping
            :deadline (+ (zara::%monotonic-seconds) 1.0d0)))
         (second
           (zara::make-%outbound
            :message
            (zara::%make-protocol-message
             :type "ping"
             :id "queued-2"
             :timestamp-ns 1)
            :promise (zara::%make-promise)
            :kind :ping
            :deadline (+ (zara::%monotonic-seconds) 1.0d0))))
    (zara::%enqueue-outbound client first)
    (signals zara:client-backpressure
      (zara::%enqueue-outbound client second))
    (is (= 1 (length (zara::client-outbound-queue client))))
    (is (eq first (car (zara::client-outbound-queue client))))))

(test event-queue-limit-is-bounded
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-event-limit.sock"
            :event-limit 1))
         (first
           (zara::%make-protocol-message
            :type "assistant.started"
            :id "event-1"
            :timestamp-ns 1))
         (second
           (zara::%make-protocol-message
            :type "assistant.started"
            :id "event-2"
            :timestamp-ns 2)))
    (zara::%enqueue-event client first)
    (zara::%enqueue-event client second)
    (is (= 1 (length (zara::client-event-queue client))))
    (is (string= "event-2"
                 (zara:protocol-message-id
                  (car (zara::client-event-queue client)))))))

(test restart-clears-stale-client-generation
  (let* ((client
           (zara:make-client
            :endpoint "ipc:///tmp/zara-test-restart.sock"))
         (promise (zara::%make-promise))
         (message
           (zara::%make-protocol-message
            :type "ping"
            :id "stale-request"
            :timestamp-ns 1)))
    (setf (zara::client-session-id client) "stale-session"
          (zara::client-conversation-id client) "stale-conversation")
    (zara::%enqueue-outbound
     client
     (zara::make-%outbound
      :message message
      :promise promise
      :kind :ping
      :deadline (+ (zara::%monotonic-seconds) 1.0d0)))
    (zara::%enqueue-event client message)
    (setf (gethash "stale-request" (zara::client-pending client))
          (zara::make-%pending
           :promise promise
           :kind :ping
           :deadline (+ (zara::%monotonic-seconds) 1.0d0)))
    (is (eq :start (zara::%prepare-client-start client)))
    (is (eq :starting (zara:client-state client)))
    (is (null (zara:client-session-id client)))
    (is (null (zara:client-conversation-id client)))
    (is (null (zara::client-outbound-queue client)))
    (is (null (zara::client-event-queue client)))
    (is (zerop (hash-table-count (zara::client-pending client))))))
