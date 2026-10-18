(asdf:defsystem #:zara
  :version "0.1.0"
  :description "Common Lisp client and server lifecycle API for Zara's ZARA/1 protocol."
  :author "lost-rob0t / StarIntel"
  :license "GPL-3.0-or-later"
  :serial t
  :depends-on (#:pzmq #:com.inuoe.jzon #:babel #:bordeaux-threads)
  :components
  ((:module "src" :serial t
    :components
    ((:file "package")
     (:file "protocol")
     (:file "client-state")
     (:file "client-transport")
     (:file "client-api")
     (:file "server"))))
  :in-order-to ((test-op (test-op #:zara/tests))))

(asdf:defsystem #:zara/tests
  :version "0.1.0"
  :depends-on (#:zara #:fiveam)
  :serial t
  :components
  ((:module "t" :serial t
    :components
    ((:file "package")
     (:file "protocol-test")
     (:file "client-test")
     (:file "server-test"))))
  :perform
  (test-op (operation component)
    (declare (ignore operation component))
    (unless (uiop:symbol-call :zara/tests :run-tests)
      (error "Zara Common Lisp tests failed"))))
