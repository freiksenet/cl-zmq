(include "zmq.h")

(in-package :cl-zmq)

;; Maximal size of "Very Small Message". VSMs are passed by value
;; to avoid excessive memory allocation/deallocation.
;; If VMSs larger than 255 bytes are required, type of 'vsm_size'
;; field in zmq_msg_t structure should be modified accordingly.
(constant (zmq-max-vsm-size "ZMQ_MAX_VSM_SIZE"))

;; Message & notification types.
(constant (zmq-gap "ZMQ_GAP"))
(constant (zmq-delimiter "ZMQ_DELIMITER"))
(constant (zmq-vsm "ZMQ_VSM"))

;; Socket options.
(constant (zmq-hwm "ZMQ_HWM"))			 ;;  int64_t
(constant (zmq-lwm "ZMQ_LWM"))			 ;;  int64_t
(constant (zmq-swap "ZMQ_SWAP"))                 ;;  int64_t
(constant (zmq-affinity "ZMQ_AFFINITY"))	 ;;  int64_t
(constant (zmq-identity "ZMQ_IDENTITY"))	 ;;  string
(constant (zmq-subscribe "ZMQ_SUBSCRIBE"))	 ;;  string
(constant (zmq-unsubscribe "ZMQ_UNSUBSCRIBE"))	 ;;  string
(constant (zmq-rate "ZMQ_RATE"))                 ;;  int64_t
(constant (zmq-recovery-ivl "ZMQ_RECOVERY_IVL")) ;;  int64_t
(constant (zmq-mcast-loop "ZMQ_MCAST_LOOP"))	 ;;  int64_t

;; The operation should be performed in non-blocking mode. I.e. if it cannot
;; be processed immediately, error should be returned with errno set to EAGAIN.
(constant (zmq-noblock "ZMQ_NOBLOCK"))

;; zmq_send should not flush the message downstream immediately. Instead, it
;; should batch ZMQ_NOFLUSH messages and send them downstream only if zmq_flush
;; is invoked. This is an optimisation for cases where several messages are
;; sent in a single business transaction. However, the effect is measurable
;; only in extremely high-perf scenarios (million messages a second or so).
;; If that's not your case, use standard flushing send instead. See exchange
;; example for illustration of ZMQ_NOFLUSH functionality.
(constant (zmq-noflush "ZMQ_NOFLUSH"))

;; Socket to communicate with a single peer. Allows for a singe connect or a
;; single accept. There's no message routing or message filtering involved.
(constant (zmq-p2p "ZMQ_P2P"))

;; Socket to distribute data. Recv fuction is not implemented for this socket
;; type. Messages are distributed in fanout fashion to all peers.
(constant (zmq-pub "ZMQ_PUB"))

;; Socket to subscribe to distributed data. Send function is not implemented
;; for this socket type. However, subscribe function can be used to modify the
;; message filter.
(constant (zmq-sum "ZMQ_SUB"))

;; Socket to send requests on and receive replies from. Requests are
;; load-balanced among all the peers. This socket type doesn't allow for more
;; recv's that there were send's.
(constant (zmq-req "ZMQ_REQ"))

;; Socket to receive requests from and send replies to. This socket type allows
;; only an alternated sequence of recv's and send's. Each send is routed to
;; the peer that the previous recv delivered message from.
(constant (zmq-rep "ZMQ_REP"))
