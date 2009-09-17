(in-package :cl-zmq)

;; prototype for the message body deallocation functions.
;; it is deliberately defined in the way to comply with standard c free.
;; typedef void (zmq_free_fn) (void *data);

;; a message. if 'shared' is true, message content pointed to by 'content'
;; is shared, i.e. reference counting is used to manage its lifetime
;; rather than straighforward malloc/free. struct zmq_msg_content is
;; not declared in the api.
(defcstruct zmq-msg
  (content	:pointer)
  (shared	:uchar)
  (vsm-size	:uchar)
  (vsm-data	:uchar :count 30))	; FIXME zmq-max-vsm-size

(defcfun ("zmq_msg_init" zmq-msg-init) :int
  "Initialise an empty message (zero bytes long)."
  (msg	zmq-msg))

(defcfun ("zmq_msg_init_size" zmq-msg-init-size) :int
  "Initialise a message 'size' bytes long.

Errors: ENOMEM - the size is too large to allocate."
  (msg	zmq-msg)
  (size	:long))

(defcfun ("zmq_msg_init_data" zmq-msg-init-data) :int
  "Initialise a message from an existing buffer. Message isn't copied,
instead 0MQ infrastructure take ownership of the buffer and call
deallocation functio (ffn) once it's not needed anymore."
  (msg	zmq-msg)
  (data	:pointer)
  (size	:long)
  (ffn	:pointer))			; zmq_free_fn

(defcfun ("zmq_msg_close" zmq-msg-close) :int
  "Deallocate the message."
  (msg	zmq-msg))

(defcfun ("zmq_msg_move" zmq-msg-move) :int
  "Move the content of the message from 'src' to 'dest'. The content isn't
copied, just moved. 'src' is an empty message after the call. Original
content of 'dest' message is deallocated."
  (dest	zmq-msg)
  (src	zmq-msg))

(defcfun ("zmq_msg_copy" zmq-msg-copy) :int
  "Copy the 'src' message to 'dest'. The content isn't copied, instead
reference count is increased. Don't modify the message data after the
call as they are shared between two messages. Original content of 'dest'
message is deallocated."
  (dest	zmq-msg)
  (src	zmq-msg))

(defcfun ("zmq_msg_data" zmq-msg-data) :int
  "Returns pointer to message data."
  (msg	zmq-msg))

(defcfun ("zmq_msg_size" zmq-msg-size) :int
  "Return size of message data (in bytes)."
  (msg	zmq-msg))

(defcfun ("zmq_msg_type" zmq-msg-type) :int
  "Returns type of the message."
  (msg	zmq-msg))

(defcfun ("zmq_init" zmq-init) :pointer
  "Initialise 0MQ context. 'app_threads' specifies maximal number
of application threads that can have open sockets at the same time.
'io_threads' specifies the size of thread pool to handle I/O operations.

Errors: EINVAL - one of the arguments is less than zero or there are no
                 threads declared at all."
  (app-threads	:int)
  (io-threads	:int))

(defcfun ("zmq_term" zmq-term) :int
  "Deinitialise 0MQ context including all the open sockets. Closing
sockets after zmq_term has been called will result in undefined behaviour."
  (context	:pointer))

(defcfun ("zmq_socket" zmq-socket) :pointer
  "Open a socket.

Errors: EINVAL - invalid socket type.
        EMFILE - the number of application threads entitled to hold open
                 sockets at the same time was exceeded."
  (context	:pointer)
  (type		:int))

(defcfun ("zmq_close" zmq-close) :int
  "Close the socket."
  (s	:pointer))

(defcfun ("zmq_setsockopt" zmq-setsockopt) :int
  "Sets an option on the socket.
EINVAL - unknown option, a value with incorrect length or an invalid value."
  (s		:pointer)
  (option	:int)
  (optval	:int)
  (optvallen	:long))

(defcfun ("zmq_bind" zmq-bind) :int
  "Bind the socket to a particular address."
  (s	:pointer)
  (addr	:pointer :char))

(defcfun ("zmq_connect" zmq-connect) :int
  "Connect the socket to a particular address."
  (s	:pointer)
  (addr	:pointer :char))

(defcfun ("zmq_send" zmq-send) :int
  "Send the message 'msg' to the socket 's'. 'flags' argument can be
combination of following values:
ZMQ_NOBLOCK - if message cannot be sent, return immediately.
ZMQ_NOFLUSH - message won't be sent immediately. It'll be sent with either
              subsequent flushing send or explicit call to zmq_flush
              function.

Errors: EAGAIN - message cannot be sent at the moment (applies only to
                 non-blocking send).
        EFAULT - function isn't supported by particular socket type."
  (s		:pointer)
  (msg		zmq-msg)
  (flags	:int))

(defcfun ("zmq_flush" zmq-flush) :int
  "Flush the messages that were send using ZMQ_NOFLUSH flag down the stream.

Errors: FAULT - function isn't supported by particular socket type."
  (s	:pointer))

(defcfun ("zmq_recv" zmq-recv) :int
  "Receive a message from the socket 's'. 'flags' argument can be combination
of following values:
ZMQ_NOBLOCK - if message cannot be received, return immediately.

Errors: EAGAIN - message cannot be received at the moment (applies only to
                 non-blocking receive).
        EFAULT - function isn't supported by particular socket type."
  (s		:pointer)
  (msg		zmq-msg)
  (flags	:int))

;; Helper functions used by perf tests so that they don't have to care
;; about minutiae of time-related functions on different OS platforms.

(defcfun ("zmq_stopwatch_start" zmq-stopwatch-start) :pointer)
(defcfun ("zmq_stopwatch_stop" zmq-stopwatch-stop) :ulong
  (watch	:pointer))
(defcfun ("zmq_sleep" zmq-sleep) :void
  (seconds	:int))
