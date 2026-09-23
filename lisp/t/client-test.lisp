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
