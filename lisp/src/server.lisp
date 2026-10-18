(in-package :zara)

(defstruct (server (:constructor %make-server))
  process
  endpoint
  runtime-dir
  executable)

(defun %default-runtime-dir ()
  (merge-pathnames
   (format nil "zara-cl-~a/" (%message-id))
   (uiop:temporary-directory)))

(defun %endpoint-for-runtime-dir (runtime-dir)
  (let* ((directory (uiop:ensure-directory-pathname runtime-dir))
         (socket (merge-pathnames "zara-server.sock" directory)))
    (format nil "ipc://~a" (uiop:native-namestring socket))))

(defun server-alive-p (server)
  (and (server-process server)
       (ignore-errors (uiop:process-alive-p (server-process server)))))

(defun %probe-server (server timeout)
  (let ((deadline (+ (%monotonic-seconds) timeout))
        (last-error nil))
    (loop
      (unless (server-alive-p server)
        (error "zara-server exited before becoming ready~@[ (~a)~]" last-error))
      (let ((remaining (- deadline (%monotonic-seconds))))
        (when (<= remaining 0)
          (error 'zara-timeout :operation "server readiness"))
        (let ((client (make-client :endpoint (server-endpoint server)
                                   :request-timeout (min 0.25d0 remaining))))
          (handler-case
              (progn
                (start-client client :timeout (min 0.25d0 remaining))
                (ping client :timeout (min 0.25d0 remaining))
                (close-client client)
                (return t))
            (error (condition)
              (setf last-error condition)
              (ignore-errors (close-client client))
              (sleep 0.05))))))))

(defun start-server (&key
                       (executable "zara-server")
                       runtime-dir
                       endpoint
                       (startup-timeout 10.0d0)
                       (shutdown-timeout 5.0d0)
                       verbose)
  "Launch the packaged Python Zara server and wait for a real ZARA/1 handshake.
The returned SERVER owns the child process and should be stopped with STOP-SERVER."
  (let* ((runtime-dir (uiop:ensure-directory-pathname
                       (or runtime-dir (%default-runtime-dir))))
         (endpoint (or endpoint (%endpoint-for-runtime-dir runtime-dir))))
    (unless (uiop:string-prefix-p "ipc://" endpoint)
      (error "START-SERVER intentionally supports local IPC only"))
    (let* ((command (append (list executable
                                  "--runtime-dir" (uiop:native-namestring runtime-dir)
                                  "--endpoint" endpoint
                                  "--shutdown-timeout"
                                  (princ-to-string shutdown-timeout))
                            (when verbose (list "--verbose"))))
           (process (uiop:launch-program command
                                         :input :null
                                         :output :interactive
                                         :error-output :interactive
                                         :wait nil))
           (server (%make-server :process process
                                 :endpoint endpoint
                                 :runtime-dir runtime-dir
                                 :executable executable)))
      (handler-case
          (progn
            (%probe-server server startup-timeout)
            server)
        (error (condition)
          (ignore-errors (stop-server server))
          (error condition))))))

(defun stop-server (server)
  "Terminate SERVER if it is still running. Idempotent."
  (check-type server server)
  (let ((process (server-process server)))
    (when (and process (ignore-errors (uiop:process-alive-p process)))
      (ignore-errors (uiop:terminate-process process))
      (ignore-errors (uiop:wait-process process)))
    (setf (server-process server) nil))
  server)

(defmacro with-server ((name &rest options) &body body)
  `(let ((,name (start-server ,@options)))
     (unwind-protect
          (progn ,@body)
       (stop-server ,name))))
