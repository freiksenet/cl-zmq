(in-package #:zmq.tests)

(defun run-tests ()
  (run! 'main))

(def-suite main)

(in-suite main)

(defparameter *test-endpoint* "tcp://127.0.0.1:31723")

;; Version

(test version
  "Version should return"
  (is (not (null (zmq:version)))))

;: Creating and configuring contexts

(test ctx
  "Context should be creatable and destroyable."
  (let ((ctx (zmq:ctx-new)))
    (is (not (null ctx)))
    (is (zerop (zmq:ctx-destroy ctx)))
    (signals (zmq:zmq-error
              "Destroyed context shouldn't be destroyable")
      (zmq:ctx-destroy ctx))))

(test ctx-get
  "Context settings should be retrievable."
  (zmq:with-context (ctx)
    (let ((ctx (zmq:ctx-new)))
      (is (= 1 (zmq:ctx-get ctx :io-threads)))
      (is (= 1024 (zmq:ctx-get ctx :max-sockets)))
      (signals (zmq:zmq-error
                "Invalid setting keywords should signal an error")
        (zmq:ctx-get ctx :invalid-stuff)))))

(test ctx-set
  "Context setting should be changeable."
  (zmq:with-context (ctx)
    (is (zerop (zmq:ctx-set ctx :io-threads 10)))
    (is (= 10 (zmq:ctx-get ctx :io-threads))
        "Setting settings should make getting return appropriately")
    (signals (zmq:zmq-error
              "Invalid setting keywords should signal an error")
      (zmq:ctx-set ctx :invalid-stuff 10))))

(test ctx-abnormal-get-set
  "Accesing settings of destroyed context should signal an error"
  (let ((ctx (zmq:ctx-new)))
    (zmq:ctx-destroy ctx)
    (signals (zmq:zmq-error)
      (zmq:ctx-set ctx :io-threads 10))
    (signals (zmq:zmq-error)
      (zmq:ctx-get ctx :max-sockets))))

;; Creating and configuring sockets

(test socket
  "Sockets should be creatable and closable"
  (zmq:with-context (ctx)
    (let ((socket (zmq:socket ctx :pub)))
      (is (not (null socket)))
      (is (zerop (zmq:close socket)))
      (signals (zmq:zmq-error
                "Closed sockets can't be closed again")
        (zmq:close socket)))))

(test socket-abnormal-creation
  "Abnormal socket creation should signal error."
  (let ((ctx (zmq:ctx-new)))
    (signals (zmq:zmq-error
              "Creating socket with wrong type should signal error")
      (zmq:socket ctx :invalid-type))
    (zmq:ctx-destroy ctx)
    (signals (zmq:zmq-error
              "Socket can't be created from closed context")
      (zmq:socket ctx :pub))))

(test socket-bind
  "Socket should be bindable and unbindable."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :push)
      (is (zerop (zmq:bind s *test-endpoint*)))
      (signals (zmq:zmq-error
                "Can't bind to one endpoint twice")
        (zerop (zmq:bind s *test-endpoint*)))
      (is (zerop (zmq:unbind s *test-endpoint*)))
      (signals (zmq:zmq-error
                "Socket can not be unbound if not bound")
        (zerop (zmq:unbind s *test-endpoint*))))))

(test socket-connect
  "Socket should be able to connect and disconnect."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :push)
      (is (zerop (zmq:connect s *test-endpoint*)))
      (is (zerop (zmq:disconnect s *test-endpoint*)))
      (signals (zmq:zmq-error
                "Socket can not be connected if it is not")
        (zerop (zmq:disconnect s *test-endpoint*))))))

(test socket-get
  "Socket settings should be retrievable."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pub)
      (is (= 0 (zmq:getsockopt s :affinity)))
      (signals (zmq:zmq-error
                "Invalid setting keywords should signal an error.")
        (zmq:getsockopt s :invalid-stuff)))))

(test socket-set
  "Socket settings should be settable."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pub)
      (is (zerop (zmq:setsockopt s :affinity 10)))
      (is (= 10 (zmq:getsockopt s :affinity)))
      (signals (zmq:zmq-error
                "Invalid setting keywords should signal an error")
        (zmq:setsockopt s :invalid-stuff 10)))))

(test socket-get-set-string
  "Socket settings that are strings should work too."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pub)
      (is (zerop (zmq:setsockopt s :identity "Foobar")))
      (is (equal "Foobar" (zmq:getsockopt s :identity))))))
