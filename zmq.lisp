(in-package :cl-zmq)

(defcvar "errno" :int)

(defmacro defcfun* (name-and-options return-type &body args)
  (let* ((c-name (car name-and-options))
	 (l-name (cadr name-and-options))
	 (n-name (cffi::format-symbol t "%~A" l-name))
	 (name (list c-name n-name))

	 (docstring (when (stringp (car args)) (pop args)))
	 (ret (gensym)))
    (loop with opt
       for i in args
       unless (consp i) do (setq opt t)
       else
       collect i into args*
       and if (not opt) collect (car i) into names
       else collect (car i) into opts
       and collect (list (car i) 0) into opts-init
       end
       finally (return
	 `(progn
	    (defcfun ,name ,return-type
	      ,@args*)

	    (defun ,l-name (,@names &optional ,@opts-init)
	      ,docstring
	      (let ((,ret (,n-name ,@names ,@opts)))
		(when ,(if (eq return-type :pointer)
			   `(zerop (pointer-address ,ret))
			   `(not (zerop ,ret)))
		  (error (convert-from-foreign (%strerror *errno*) :string)))
		,ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant hausnumero 156384712
  "A number random anough not to collide with different errno ranges on
different OSes. The assumption is that error_t is at least 32-bit type.")

;;  On Windows platform some of the standard POSIX errnos are not defined.
;; #ifndef ENOTSUP
;; #define ENOTSUP (ZMQ_HAUSNUMERO + 1)
;; #endif
;; #ifndef EPROTONOSUPPORT
;; #define EPROTONOSUPPORT (ZMQ_HAUSNUMERO + 2)
;; #endif

;;  Native 0MQ error codes.
(defconstant emthread (+ hausnumero 50))
(defconstant efsm (+ hausnumero 51))
(defconstant enocompatproto (+ hausnumero 52))

(defcfun ("zmq_strerror" %strerror) :pointer
  "Resolves system errors and 0MQ errors to human-readable string."
  (errnum	:int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ message definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant max-vsm-size 30
  "Maximal size of \"Very Small Message\". VSMs are passed by value
to avoid excessive memory allocation/deallocation.
If VMSs larger than 255 bytes are required, type of 'vsm_size'
field in zmq_msg_t structure should be modified accordingly.")

;;  Message types. These integers may be stored in 'content' member of the
;;  message instead of regular pointer to the data.
(defconstant delimiter 31)
(defconstant vsm 32)

(defcstruct msg
    "A message. if 'shared' is true, message content pointed to by 'content'
is shared, i.e. reference counting is used to manage its lifetime
rather than straighforward malloc/free. struct zmq_msg_content is
not declared in the api."
  (content	:pointer)
  (shared	:uchar)
  (vsm-size	:uchar)
  (vsm-data	:uchar :count 30))	;; FIXME max-vsm-size

(defcfun ("zmq_msg_init" msg-init) :int
  "Initialise an empty message (zero bytes long)."
  (msg	msg))

(defcfun* ("zmq_msg_init_size" msg-init-size) :int
  "Initialise a message 'size' bytes long.

Errors: ENOMEM - the size is too large to allocate."
  (msg	msg)
  (size	:long))

;;typedef void (zmq_free_fn) (void *data);
(defcfun ("zmq_msg_init_data" msg-init-data) :int
  "Initialise a message from an existing buffer. Message isn't copied,
instead 0MQ infrastructure takes ownership of the buffer and
deallocation function (ffn) will be called once the data are not
needed anymore. Note that deallocation function prototype is designed
so that it complies with standard C 'free' function."
  (msg	msg)
  (data	:pointer)
  (size	:long)
  (ffn	:pointer))			; zmq_free_fn

(defcfun ("zmq_msg_close" msg-close) :int
  "Deallocate the message."
  (msg	msg))

(defcfun ("zmq_msg_move" msg-move) :int
  "Move the content of the message from 'src' to 'dest'. The content isn't
copied, just moved. 'src' is an empty message after the call. Original
content of 'dest' message is deallocated."
  (dest	msg)
  (src	msg))

(defcfun ("zmq_msg_copy" msg-copy) :int
  "Copy the 'src' message to 'dest'. The content isn't copied, instead
reference count is increased. Don't modify the message data after the
call as they are shared between two messages. Original content of 'dest'
message is deallocated."
  (dest	msg)
  (src	msg))

(defcfun ("zmq_msg_data" msg-data) :pointer
  "Returns pointer to message data."
  (msg	msg))

(defcfun ("zmq_msg_size" msg-size) :int
  "Return size of message data (in bytes)."
  (msg	msg))

(defcfun ("zmq_msg_type" msg-type) :long
  "Returns type of the message."
  (msg	msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ infrastructure (a.k.a. context) initialisation & termination.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant poll 1
  "Flag specifying that the sockets within this context should be pollable.
This may be a little less efficient that raw non-pollable sockets.")

(defcfun* ("zmq_init" init) :pointer
  "Initialise 0MQ context. 'app_threads' specifies maximal number
of application threads that can own open sockets at the same time.
'io_threads' specifies the size of thread pool to handle I/O operations.
'flags' argument is a bitmap composed of the flags defined above.

Errors: EINVAL - one of the arguments is less than zero or there are no
                 threads declared at all."
  (app-threads	:int)
  (io-threads	:int)
  (flags	:int))

(defcfun ("zmq_term" term) :int
  "Deinitialise 0MQ context. If there are still open sockets, actual
deinitialisation of the context is delayed till all the sockets are closed."
  (context	:pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ socket definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Creating a 0MQ socket.
;;  **********************

(defconstant p2p 0
  "Socket to communicate with a single peer. Allows for a singe connect or a
single accept. There's no message routing or message filtering involved.")

(defconstant pub 1
  "Socket to distribute data. Recv fuction is not implemented for this socket
type. Messages are distributed in fanout fashion to all the peers.")

(defconstant sub 2
  "Socket to subscribe for data. Send function is not implemented for this
socket type. However, subscribe function can be used to modify the
message filter (see ZMQ_SUBSCRIBE socket option).")

(defconstant req 3
  "Socket to send requests and receive replies. Requests are
load-balanced among all the peers. This socket type allows
only an alternated sequence of send's and recv's")

(defconstant rep 4
  "Socket to receive requests and send replies. This socket type allows
only an alternated sequence of recv's and send's. Each send is routed to
the peer that issued the last received request.")

(defcfun* ("zmq_socket" socket) :pointer
  "Open a socket.

Errors: EINVAL - invalid socket type.
        EMFILE - the number of application threads entitled to hold open
                 sockets at the same time was exceeded."
  (context	:pointer)
  (type		:int))

;;  Destroying the socket.
;;  **********************

(defcfun ("zmq_close" close) :int
  "Close the socket."
  (s	:pointer))

;;  Manipulating socket options.
;;  ****************************

;;  Available socket options, their types and default values.

(defconstant hwm 1
  "High watermark for the message pipes associated with the socket. The water
mark cannot be exceeded. If the messages don't fit into the pipe emergency
mechanisms of the particular socket type are used (block, drop etc.) If HWM
is set to zero, there are no limits for the content of the pipe.
Type: int64_t  Unit: bytes  Default: 0")

(defconstant lwm 2
  "Low watermark makes sense only if high watermark is defined (is non-zero).
When the emergency state is reached when messages overflow the pipe, the
emergency lasts till the size of the pipe decreases to low watermark.
At that point normal state is resumed.
Type: int64_t  Unit: bytes  Default: 0")

(defconstant swap 3
"Swap allows the pipe to exceed high watermark. However, the data are written
to the disk rather than held in the memory. While the high watermark is not
exceeded there is no disk activity involved though. The value of the option
defines maximal size of the swap file.
Type: int64_t  Unit: bytes  Default: 0")

(defconstant affinity 4
  "Affinity defines which threads in the thread pool will be used to handle
newly created sockets. This way you can dedicate some of the threads (CPUs)
to a specific work. Value of 0 means no affinity, work is distributed
fairly among the threads in the thread pool. For non-zero values, the lowest
bit corresponds to the thread 1, second lowest bit to the thread 2 etc.
Thus, value of 3 means that from now on newly created sockets will handle
I/O activity exclusively using threads no. 1 and 2.
Type: int64_t  Unit: N/A (bitmap)  Default: 0")

(defconstant identity 5
  "Identity of the socket. Identity is important when restarting applications.
If the socket has no identity, each run of the application is completely
separated from other runs. However, with identity application reconnects to
existing infrastructure left by the previous run. Thus it may receive
messages that were sent in the meantime, it shares pipe limits with the
previous run etc.
Type: string  Unit: N/A  Default: NULL")

(defconstant subscribe 6
  "Applicable only to 'sub' socket type. Eastablishes new message filter.
When 'sub' socket is created all the incoming messages are filtered out.
This option allows you to subscribe for all messages (\"*\"), messages with
specific topic (\"x.y.z\") and/or messages with specific topic prefix
(\"x.y.*\"). Topic is one-byte-size-prefixed string located at
the very beginning of the message. Multiple filters can be attached to
a single 'sub' socket. In that case message passes if it matches at least
one of the filters.
Type: string  Unit: N/A  Default: N/A")

(defconstant unsubscribe 7
  "Applicable only to 'sub' socket type. Removes existing message filter.
The filter specified must match the string passed to ZMQ_SUBSCRIBE options
exactly. If there were several instances of the same filter created,
this options removes only one of them, leaving the rest in place
and functional.
Type: string  Unit: N/A  Default: N/A")

(defconstant rate 8
  "This option applies only to multicast transports (pgm & udp). It specifies
maximal outgoing data rate that an individual sender socket can send.
Type: uint64_t  Unit: kilobits/second  Default: 100")

(defconstant recovery-ivl 9
  "This option applies only to multicast transports (pgm & udp). It specifies
how long can the receiver socket survive when the sender is inaccessible.
Keep in mind that large recovery intervals at high data rates result in
very large recovery buffers, meaning that you can easily overload your box
by setting say 1 minute recovery interval at 1Gb/s rate (requires
7GB in-memory buffer).
Type: uint64_t Unit: seconds Default: 10")

(defconstant mcast-loop 10
  "This option applies only to multicast transports (pgm & udp). Value of 1
means that the mutlicast packets can be received on the box they were sent
from. Setting the value to 0 disables the loopback functionality which
can have negative impact on the performance. if possible, disable
the loopback in production environments.
Type: uint64_t Unit: N/A (boolean value) Default: 1")

(defcfun* ("zmq_setsockopt" setsockopt) :int
  "Sets an option on the socket. 'option' argument specifies the option (see
the option list above). 'optval' is a pointer to the value to set,
'optvallen' is the size of the value in bytes.

Errors: EINVAL - unknown option, a value with incorrect length
                 or invalid value."
  (s		:pointer)
  (option	:int)
  (optval	:pointer)
  (optvallen	:long))

;;  Creating connections.
;;  *********************

;;  Addresses are composed of the name of the protocol to use followed by ://
;;  and a protocol-specific address. Available protocols:
;;
;;  tcp - the address is composed of IP address and port delimited by colon
;;        sign (:). The IP address can be a hostname (with 'connect') or
;;        a network interface name (with 'bind'). Examples "tcp://eth0:5555",
;;        "tcp://192.168.0.1:20000", "tcp://hq.mycompany.com:80".
;;
;;  pgm & udp - both protocols have same address format. It's network interface
;;              to use, semicolon (;), multicast group IP address, colon (:) and
;;              port. Examples: "pgm://eth2;224.0.0.1:8000",
;;              "udp://192.168.0.111;224.1.1.1:5555".

(defcfun ("zmq_bind" %bind) :int
  "Bind the socket to a particular address.

Errors: EPROTONOSUPPORT - unsupported protocol.
        ENOCOMPATPROTO - protocol is not compatible with the socket type."
  (s	:pointer)
  (addr	:pointer :char))

(defcfun ("zmq_connect" %connect) :int
  "Connect the socket to a particular address.

Errors: EPROTONOSUPPORT - unsupported protocol.
        ENOCOMPATPROTO - protocol is not compatible with the socket type."
  (s	:pointer)
  (addr	:pointer :char))

;;  Sending and receiving messages.
;;  *******************************

(defconstant noblock 1
  "The flag specifying that the operation should be performed in
non-blocking mode. I.e. if it cannot be processed immediately,
error should be returned with errno set to EAGAIN.")

(defconstant noflush 2
  "The flag specifying that zmq_send should not flush the message downstream
immediately. Instead, it should batch ZMQ_NOFLUSH messages and send them
downstream only if zmq_flush is invoked. This is an optimisation for cases
where several messages are sent in a single business transaction. However,
the effect is measurable only in extremely high-perf scenarios
(million messages a second or so). If that's not your case, use standard
flushing send instead.")

(defcfun* ("zmq_send" send) :int
  "Send the message 'msg' to the socket 's'. 'flags' argument can be
combination the flags described above.

Errors: EAGAIN - message cannot be sent at the moment (applies only to
                 non-blocking send).
        ENOTSUP - function isn't supported by particular socket type.
        EFSM - function cannot be called at the moment."
  (s		:pointer)
  (msg		msg)
  :optional
  (flags	:int))

(defcfun* ("zmq_flush" flush) :int
  "Flush the messages that were send using ZMQ_NOFLUSH flag down the stream.

Errors: ENOTSUP - function isn't supported by particular socket type.
        EFSM - function cannot be called at the moment."
  (s	:pointer))

(defcfun* ("zmq_recv" recv) :int
  "Receive a message from the socket 's'. 'flags' argument can be combination
of the flags described above with the exception of ZMQ_NOFLUSH.

Errors: EAGAIN - message cannot be received at the moment (applies only to
                 non-blocking receive).
        ENOTSUP - function isn't supported by particular socket type.
        EFSM - function cannot be called at the moment."
  (s		:pointer)
  (msg		msg)
  :optional
  (flags	:int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper functions used by perf tests so that they don't have to care
;; about minutiae of time-related functions on different OS platforms.

(defcfun ("zmq_stopwatch_start" stopwatch-start) :pointer
  "Starts the stopwatch. Returns the handle to the watch")

(defcfun ("zmq_stopwatch_stop" stopwatch-stop) :ulong
  "Stops the stopwatch. Returns the number of microseconds elapsed since
the stopwatch was started."
  (watch	:pointer))

(defcfun ("zmq_sleep" sleep) :void
  "Sleeps for specified number of seconds."
  (seconds	:int))
