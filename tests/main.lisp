(in-package #:zmq.tests)

(defun run-tests ()
  (run! 'main))

(def-suite main)

(in-suite main)

(defparameter *test-endpoint* "tcp://127.0.0.1:31723")

(defmacro with-test-reciever (socket &body body)
  (let ((result-var (gensym "result"))
        (msg-var (gensym "msg"))
        (thread-var (gensym "thread")))
    `(let* ((,result-var)
            (,thread-var
              (bordeaux-threads:make-thread
               (lambda ()
                 (let ((,msg-var (zmq:make-msg)))
                   (zmq:msg-recv ,socket ,msg-var)
                   (setf ,result-var ,msg-var))))))
       ,@body
       (bordeaux-threads:join-thread ,thread-var)
       ,result-var)))

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
    (zmq:with-context (ctx2)
      (is (= (zmq:ctx-get ctx :io-threads)
             (zmq:ctx-get ctx2 :io-threads)))
      (is (= (zmq:ctx-get ctx :max-sockets)
             (zmq:ctx-get ctx2 :max-sockets)))
      (signals (simple-error
                "Invalid setting keywords should signal an error")
        (zmq:ctx-get ctx :invalid-stuff)))))

(test ctx-set
  "Context setting should be changeable."
  (zmq:with-context (ctx)
    (is (zerop (zmq:ctx-set ctx :io-threads 10)))
    (is (= 10 (zmq:ctx-get ctx :io-threads))
        "Setting settings should make getting return appropriately")
    (signals (simple-error
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
    (signals (simple-error
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
      (signals (simple-error
                "Invalid setting keywords should signal an error.")
        (zmq:getsockopt s :invalid-stuff)))))

(test socket-set
  "Socket settings should be settable."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pub)
      (is (zerop (zmq:setsockopt s :affinity 10)))
      (is (= 10 (zmq:getsockopt s :affinity)))
      (signals (simple-error
                "Invalid setting keywords should signal an error")
        (zmq:setsockopt s :invalid-stuff 10)))))

(test socket-get-set-string
  "Socket settings that are strings should work too."
  (zmq:with-context (ctx)
    (zmq:with-socket (s ctx :pub)
      (is (zerop (zmq:setsockopt s :identity "Foobar")))
      (is (equal "Foobar" (zmq:getsockopt s :identity))))))

;; Making and working with messages

(test make-msg-empty
  "Calling make-msg without arguments yield empty message and it's really
   empty."
  (let ((msg (zmq:make-msg)))
    (is (zerop (zmq:msg-size msg)))
    (is (equalp #() (zmq:msg-data-as-array msg)))
    (is (equal "" (zmq:msg-data-as-string msg)))))

(test make-msg-string
  "Calling make-msg with string as :data should produce message with payload
   matching the string."
  (let* ((empty-msg (zmq:make-msg :data ""))
         (content "payload of message")
         (content-vec (map 'vector #'char-code content))
         (msg (zmq:make-msg :data content)))
    (is (zerop (zmq:msg-size empty-msg)))
    (is (equalp #() (zmq:msg-data-as-array empty-msg)))
    (is (equal "" (zmq:msg-data-as-string empty-msg)))
    (is (= (length content) (zmq:msg-size msg)))
    (is (equal content (zmq:msg-data-as-string msg)))
    (is (equalp content-vec (zmq:msg-data-as-array msg)))))

(test make-msg-string-unicode
  "Calling make-msg with unicode data should produce message with payload
   encoded correctly."
  (let ((msg (zmq:make-msg :data "ю")))
    (is (= 2 (zmq:msg-size msg)))
    (is (equal "ю" (zmq:msg-data-as-string msg)))))

(test make-msg-array
  "Calling make-msg with array as data should produce message with matching
   payload."
  (let ((msg (zmq:make-msg :data #(1 2 3 4))))
    (is (= 4 (zmq:msg-size msg)))
    (is (equalp #(1 2 3 4) (zmq:msg-data-as-array msg)))))

(test make-msg-size
  "Calling make-msg with size argument should produce message of appropriate
   size."
  (let ((msg (zmq:make-msg :size 10)))
    (is (= 10 (zmq:msg-size msg)))))

(test msg-copy
  "Calling msg-copy should copy the payload of the source without disturbing
   the original."
  (let ((src-msg (zmq:make-msg :data #(1 2 3 4)))
        (dst-msg (zmq:make-msg)))
    (zmq:msg-copy dst-msg src-msg)
    (is (= (zmq:msg-size src-msg) (zmq:msg-size dst-msg) 4))
    (is (equalp (zmq:msg-data-as-array src-msg)
                (zmq:msg-data-as-array dst-msg)))
    (is (equalp #(1 2 3 4)
                (zmq:msg-data-as-array src-msg)))))

(test msg-move
  "Calling msg-move should copy the payload of the source while making original
   an empty message."
  (let ((src-msg (zmq:make-msg :data #(1 2 3 4)))
        (dst-msg (zmq:make-msg)))
    (zmq:msg-move dst-msg src-msg)
    (is (equalp #() (zmq:msg-data-as-array src-msg)))
    (is (= 4 (zmq:msg-size dst-msg)))
    (is (equalp #(1 2 3 4)
                (zmq:msg-data-as-array dst-msg)))))

;; Sending and recieving the messages

(test test-send-recv-msg
  "Messages should be sent and recieved correctly with msg-send and msg-recv."
  (let ((msg (zmq:make-msg :data #(1 2 3 4))))
    (zmq:with-context (ctx)
      (zmq:with-socket (sender ctx :push)
        (zmq:with-socket (reciever ctx :pull)
          (zmq:bind sender *test-endpoint*)
          (zmq:connect reciever *test-endpoint*)
          (let ((result-msg
                   (with-test-reciever reciever
                    (zmq:msg-send sender msg))))
            (is (equalp #(1 2 3 4)
                        (zmq:msg-data-as-array result-msg)))
            (is (equalp #()
                        (zmq:msg-data-as-array msg))
                "Sent message should be emptied.")))))))
