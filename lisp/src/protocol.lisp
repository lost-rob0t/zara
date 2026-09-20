(in-package :zara)

(defconstant +unix-epoch-offset+ 2208988800)
(defconstant +max-envelope-bytes+ (* 64 1024))
(defconstant +max-payload-frames+ 16)
(defconstant +max-payload-frame-bytes+ (* 1024 1024))
(defconstant +max-payload-bytes+ (* 4 1024 1024))

(defparameter +protocol-marker+
  (babel:string-to-octets "ZARA/1" :encoding :utf-8))

(defparameter +client-message-types+
  '("hello" "ping" "runtime.status" "conversation.open" "turn.submit"
    "turn.cancel" "tool.approve" "tool.reject" "capability.snapshot"
    "device.action.accepted" "device.action.result" "device.action.error"))

(defparameter +server-message-types+
  '("hello.ok" "pong" "runtime.status.ok" "conversation.opened"
    "turn.accepted" "turn.cancel.accepted" "turn.started" "turn.completed"
    "turn.cancelled" "tool.approve.accepted" "tool.reject.accepted"
    "tool.queued" "tool.waiting" "tool.started" "tool.completed"
    "tool.failed" "tool.cancelled" "capability.snapshot.ok"
    "device.action.request" "device.action.cancel" "assistant.started"
    "assistant.delta" "assistant.completed" "assistant.response"
    "voice.speech.started" "voice.transcript.partial" "voice.speech.ended"
    "voice.transcript.final" "audio.input.started" "audio.input.accepted"
    "audio.input.committed" "audio.input.cancelled" "audio.output.start"
    "audio.output.chunk" "audio.output.done" "runtime.error"
    "runtime.stopped" "protocol.error"))

(defparameter +allowed-envelope-keys+
  '("type" "id" "reply_to" "session_id" "conversation_id" "turn_id"
    "stream_id" "seq" "timestamp_ns" "trace_id" "content_type"
    "payload_count" "flags" "body"))

(define-condition zara-error (error) ())

(define-condition protocol-error (zara-error)
  ((reason :initarg :reason :reader protocol-error-reason))
  (:report (lambda (condition stream)
             (format stream "ZARA/1 protocol error: ~a"
                     (protocol-error-reason condition)))))

(define-condition remote-error (zara-error)
  ((code :initarg :code :reader remote-error-code)
   (message :initarg :message :reader remote-error-message)
   (retryable-p :initarg :retryable-p :reader remote-error-retryable-p))
  (:report (lambda (condition stream)
             (format stream "Zara remote error ~a: ~a~:[~; (retryable)~]"
                     (remote-error-code condition)
                     (remote-error-message condition)
                     (remote-error-retryable-p condition)))))

(define-condition zara-timeout (zara-error)
  ((operation :initarg :operation :reader zara-timeout-operation))
  (:report (lambda (condition stream)
             (format stream "Timed out waiting for Zara ~a"
                     (zara-timeout-operation condition)))))

(define-condition client-not-ready (zara-error)
  ((state :initarg :state :reader client-not-ready-state))
  (:report (lambda (condition stream)
             (format stream "Zara client is not ready (state ~a)"
                     (client-not-ready-state condition)))))

(define-condition client-backpressure (zara-error)
  ((limit :initarg :limit :reader client-backpressure-limit))
  (:report (lambda (condition stream)
             (format stream "Zara client queue is full (limit ~d)"
                     (client-backpressure-limit condition)))))

(defstruct (protocol-message
            (:constructor %make-protocol-message))
  type
  id
  reply-to
  session-id
  conversation-id
  turn-id
  stream-id
  seq
  trace-id
  content-type
  timestamp-ns
  body
  (payloads #() :type vector))

(defun %protocol-fail (control &rest arguments)
  (error 'protocol-error :reason (apply #'format nil control arguments)))

(defun %now-nanoseconds ()
  (* 1000000000
     (max 0 (- (get-universal-time) +unix-epoch-offset+))))

(defvar *message-id-lock* (bt:make-lock "zara-message-id"))
(defvar *message-id-counter* 0)

(defun %message-id ()
  (bt:with-lock-held (*message-id-lock*)
    (format nil "cl-~36r-~36r-~36r-~36r"
            (get-universal-time)
            (get-internal-real-time)
            (incf *message-id-counter*)
            (random #x100000000))))

(defun %json-object-p (value)
  (hash-table-p value))

(defun %json-object (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun %json-value (object key &optional default)
  (if (%json-object-p object)
      (multiple-value-bind (value present-p) (gethash key object)
        (if present-p value default))
      default))

(defun message-body-value (message key &optional default)
  (%json-value (protocol-message-body message) key default))

(defun %put-optional (object key value)
  (when value
    (setf (gethash key object) value))
  object)

(defun %encode-envelope (message payload-count)
  (let ((object (%json-object
                 "type" (protocol-message-type message)
                 "id" (protocol-message-id message)
                 "timestamp_ns" (or (protocol-message-timestamp-ns message)
                                    (%now-nanoseconds))
                 "payload_count" payload-count)))
    (%put-optional object "reply_to" (protocol-message-reply-to message))
    (%put-optional object "session_id" (protocol-message-session-id message))
    (%put-optional object "conversation_id"
                   (protocol-message-conversation-id message))
    (%put-optional object "turn_id" (protocol-message-turn-id message))
    (%put-optional object "stream_id" (protocol-message-stream-id message))
    (when (integerp (protocol-message-seq message))
      (setf (gethash "seq" object) (protocol-message-seq message)))
    (%put-optional object "trace_id" (protocol-message-trace-id message))
    (%put-optional object "content_type" (protocol-message-content-type message))
    (%put-optional object "body" (protocol-message-body message))
    object))

(defun encode-message (message)
  "Encode MESSAGE into the exact multipart frame vector used by ZARA/1."
  (check-type message protocol-message)
  (unless (member (protocol-message-type message) +client-message-types+
                  :test #'string=)
    (%protocol-fail "unsupported client message type ~s"
                    (protocol-message-type message)))
  (let* ((payloads (protocol-message-payloads message))
         (payload-count (length payloads))
         (json (com.inuoe.jzon:stringify (%encode-envelope message payload-count)))
         (envelope (babel:string-to-octets json :encoding :utf-8)))
    (when (> (length envelope) +max-envelope-bytes+)
      (%protocol-fail "envelope exceeds ~d bytes" +max-envelope-bytes+))
    (when (> payload-count +max-payload-frames+)
      (%protocol-fail "payload frame count exceeds ~d" +max-payload-frames+))
    (let ((total 0))
      (loop for payload across payloads
            do (unless (typep payload '(vector (unsigned-byte 8)))
                 (%protocol-fail "payload frames must be octet vectors"))
               (when (> (length payload) +max-payload-frame-bytes+)
                 (%protocol-fail "payload frame exceeds byte limit"))
               (incf total (length payload)))
      (when (> total +max-payload-bytes+)
        (%protocol-fail "payload bytes exceed total byte limit")))
    (concatenate 'vector
                 (vector +protocol-marker+ envelope)
                 payloads)))

(defun %validate-envelope-keys (object)
  (loop for key being the hash-keys of object
        do (unless (member key +allowed-envelope-keys+ :test #'string=)
             (%protocol-fail "unknown JSON envelope key ~s" key))))

(defun %required-string (object key)
  (let ((value (%json-value object key nil)))
    (unless (and (stringp value) (plusp (length value)))
      (%protocol-fail "~a must be a non-empty string" key))
    value))

(defun %optional-string (object key)
  (let ((value (%json-value object key nil)))
    (when (and value (not (stringp value)))
      (%protocol-fail "~a must be a string" key))
    value))

(defun decode-message (frames)
  "Decode and validate one server-to-client ZARA/1 multipart message."
  (unless (and (vectorp frames) (>= (length frames) 2))
    (%protocol-fail "message requires protocol marker and envelope frames"))
  (unless (equalp (aref frames 0) +protocol-marker+)
    (%protocol-fail "unsupported protocol marker"))
  (let ((envelope-frame (aref frames 1)))
    (unless (typep envelope-frame '(vector (unsigned-byte 8)))
      (%protocol-fail "envelope frame must be octets"))
    (when (> (length envelope-frame) +max-envelope-bytes+)
      (%protocol-fail "envelope exceeds byte limit"))
    (let* ((object (handler-case
                       (com.inuoe.jzon:parse
                        envelope-frame
                        :max-depth 64
                        :max-string-length +max-envelope-bytes+)
                     (error ()
                       (%protocol-fail "envelope is not strict UTF-8 JSON")))))
      (unless (%json-object-p object)
        (%protocol-fail "envelope must be a JSON object"))
      (%validate-envelope-keys object)
      (let* ((type (%required-string object "type"))
             (id (%required-string object "id"))
             (timestamp (%json-value object "timestamp_ns" nil))
             (payload-count (%json-value object "payload_count" nil))
             (payloads (subseq frames 2)))
        (unless (member type +server-message-types+ :test #'string=)
          (%protocol-fail "unsupported server message type ~s" type))
        (unless (and (integerp timestamp) (>= timestamp 0))
          (%protocol-fail "timestamp_ns must be a non-negative integer"))
        (unless (and (integerp payload-count) (>= payload-count 0))
          (%protocol-fail "payload_count must be a non-negative integer"))
        (unless (= payload-count (length payloads))
          (%protocol-fail "payload_count does not match multipart frame count"))
        (%make-protocol-message
         :type type
         :id id
         :reply-to (%optional-string object "reply_to")
         :session-id (%optional-string object "session_id")
         :conversation-id (%optional-string object "conversation_id")
         :turn-id (%optional-string object "turn_id")
         :stream-id (%optional-string object "stream_id")
         :seq (%json-value object "seq" nil)
         :trace-id (%optional-string object "trace_id")
         :content-type (%optional-string object "content_type")
         :timestamp-ns timestamp
         :body (%json-value object "body" nil)
         :payloads payloads)))))
