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

(defun %promise-reject (promise condition)
  (bt:with-lock-held ((%promise-lock promise))
    (unless (%promise-done-p promise)
      (setf (%promise-error promise) condition
            (%promise-done-p promise) t)
      (bt:condition-notify (%promise-condition promise))))
  condition)

(defun %promise-complete-p (promise)
  (bt:with-lock-held ((%promise-lock promise))
    (%promise-done-p promise)))

(defun %monotonic-seconds ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defun %promise-await (promise timeout operation)
  (let ((deadline (+ (%monotonic-seconds) timeout)))
    (bt:with-lock-held ((%promise-lock promise))
      (loop until (%promise-done-p promise)
            for remaining = (- deadline (%monotonic-seconds))
            do (when (<= remaining 0)
                 (let ((condition
                         (make-condition 'zara-timeout :operation operation)))
                   (setf (%promise-error promise) condition
                         (%promise-done-p promise) t)
                   (error condition)))
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
  "Create a bounded owner-local ZARA/1 client."
  (unless (and (stringp endpoint) (plusp (length endpoint)))
    (error "ENDPOINT must be a non-empty string"))
  (unless (uiop:string-prefix-p "ipc://" endpoint)
    (error "The Common Lisp Zara client currently supports owner-local IPC only"))
  (unless (and (realp request-timeout) (> request-timeout 0))
    (error "REQUEST-TIMEOUT must be positive"))
  (unless (and (integerp outbound-limit) (> outbound-limit 0))
    (error "OUTBOUND-LIMIT must be a positive integer"))
  (unless (and (integerp event-limit) (> event-limit 0))
    (error "EVENT-LIMIT must be a positive integer"))
  (unless (and (integerp poll-interval-ms) (> poll-interval-ms 0))
    (error "POLL-INTERVAL-MS must be a positive integer"))
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

(defun %client-state-value (client)
  (bt:with-lock-held ((client-state-lock client))
    (client-state client)))

(defun %set-client-state (client state)
  (bt:with-lock-held ((client-state-lock client))
    (setf (client-state client) state)))

(defun %stop-requested-p (client)
  (bt:with-lock-held ((client-state-lock client))
    (client-stop-p client)))

(defun %request-stop (client)
  (bt:with-lock-held ((client-state-lock client))
    (setf (client-stop-p client) t)
    (unless (member (client-state client) '(:failed :stopped))
      (setf (client-state client) :stopping))))

(defun %mark-ready (client session-id)
  (bt:with-lock-held ((client-state-lock client))
    (setf (client-session-id client) session-id
          (client-state client) :ready)))

(defun %prepare-client-start (client)
  (bt:with-lock-held ((client-state-lock client))
    (when (eq (client-state client) :ready)
      (return-from %prepare-client-start :already-ready))
    (unless (member (client-state client) '(:new :stopped :failed))
      (error 'client-not-ready :state (client-state client)))
    (setf (client-state client) :starting
          (client-stop-p client) nil
          (client-session-id client) nil
          (client-conversation-id client) nil))
  (bt:with-lock-held ((client-outbound-lock client))
    (setf (client-outbound-queue client) '()))
  (bt:with-lock-held ((client-event-lock client))
    (setf (client-event-queue client) '()))
  (clrhash (client-pending client))
  :start)

(defun %ensure-ready (client)
  (let ((state (%client-state-value client)))
    (unless (eq state :ready)
      (error 'client-not-ready :state state))))

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
      (setf (client-event-queue client)
            (nconc queue (list message)))
      (bt:condition-notify (client-event-condition client)))))

(defun %wake-event-waiter (client)
  (bt:with-lock-held ((client-event-lock client))
    (bt:condition-notify (client-event-condition client))))
