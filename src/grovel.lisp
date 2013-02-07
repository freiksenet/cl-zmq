;; This file is part of CL-ZMQ.

(include "zmq.h")
#+win32 (include "Winsock2.h")

(in-package :zmq)

(ctype size-t "size_t")

#+win32 (ctype win32-socket "SOCKET")

(constantenum error-code
              ;; Standard error codes
              ((:enotsup "ENOTSUP"))
              ((:eprotonosupport "EPROTONOSUPPORT"))
              ((:enobufs "ENOBUFS"))
              ((:enetdown "ENETDOWN"))
              ((:eaddrinuse "EADDRINUSE"))
              ((:eaddrnotavail "EADDRNOTAVAIL"))
              ((:econnrefused "ECONNREFUSED"))
              ((:einprogress "EINPROGRESS"))
              ((:enotsock "ENOTSOCK"))
              ((:emsgsize "EMSGSIZE"))
              ((:eafnosupport "EAFNOSUPPORT"))
              ((:enetunreach "ENETUNREACH"))
              ((:econnaborted "ECONNABORTED"))
              ((:enotconn "ENOTCONN"))
              ((:etimedout "ETIMEDOUT"))
              ((:ehostunreach "EHOSTUNREACH"))
              ((:enetreset "ENETRESET"))
              ((:einval "EINVAL"))
              ((:enodev "ENODEV"))
              ((:eintr "EINTR"))
              ((:efault "EFAULT"))
              ((:enomem "ENOMEM"))
              ((:eagain "EAGAIN"))
              ((:emfile "EMFILE"))
              ;; ZMQ native error codes
              ((:efsm "EFSM"))
              ((:enocompatproto "ENOCOMPATPROTO"))
              ((:eterm "ETERM"))
              ((:emthread "EMTHREAD")))

(constantenum context-options
              ((:io-threads "ZMQ_IO_THREADS"))
              ((:max-sockets "ZMQ_MAX_SOCKETS")))

(constantenum socket-type
              ((:pair "ZMQ_PAIR"))
              ((:pub "ZMQ_PUB"))
              ((:sub "ZMQ_SUB"))
              ((:req "ZMQ_REQ"))
              ((:rep "ZMQ_REP"))
              ((:dealer "ZMQ_DEALER"))
              ((:router "ZMQ_ROUTER"))
              ((:pull "ZMQ_PULL"))
              ((:push "ZMQ_PUSH"))
              ((:xpub "ZMQ_XPUB"))
              ((:xsub "ZMQ_XSUB")))

(constantenum socket-option
              ((:affinity "ZMQ_AFFINITY"))
              ((:identity "ZMQ_IDENTITY"))
              ((:subscribe "ZMQ_SUBSCRIBE"))
              ((:unsubscribe "ZMQ_UNSUBSCRIBE"))
              ((:rate "ZMQ_RATE"))
              ((:recovery-ivl "ZMQ_RECOVERY_IVL"))
              ((:sndbuf "ZMQ_SNDBUF"))
              ((:rcvbuf "ZMQ_RCVBUF"))
              ((:rcvmore "ZMQ_RCVMORE"))
              ((:fd "ZMQ_FD"))
              ((:events "ZMQ_EVENTS"))
              ((:type "ZMQ_TYPE"))
              ((:linger "ZMQ_LINGER"))
              ((:reconnect-ivl "ZMQ_RECONNECT_IVL"))
              ((:backlog "ZMQ_BACKLOG"))
              ((:reconnect-ivl-max "ZMQ_RECONNECT_IVL_MAX"))
              ((:maxmsgsize "ZMQ_MAXMSGSIZE"))
              ((:sndhwm "ZMQ_SNDHWM"))
              ((:rcvhwm "ZMQ_RCVHWM"))
              ((:multicast-hops "ZMQ_MULTICAST_HOPS"))
              ((:rcvtimeo "ZMQ_RCVTIMEO"))
              ((:sndtimeo "ZMQ_SNDTIMEO"))
              ((:ipv4only "ZMQ_IPV4ONLY"))
              ((:last-endpoint "ZMQ_LAST_ENDPOINT"))
              ((:router-mandatory "ZMQ_ROUTER_MANDATORY"))
              ((:tcp-keepalive "ZMQ_TCP_KEEPALIVE"))
              ((:tcp-keepalive-cnt "ZMQ_TCP_KEEPALIVE_CNT"))
              ((:tcp-keepalive-idle "ZMQ_TCP_KEEPALIVE_IDLE"))
              ((:tcp-keepalive-intvl "ZMQ_TCP_KEEPALIVE_INTVL"))
              ((:tcp-accept-filter "ZMQ_TCP_ACCEPT_FILTER"))
              ((:tcp-delay-attach-on-connect "ZMQ_DELAY_ATTACH_ON_CONNECT"))
              ((:xpub-verbose "ZMQ_XPUB_VERBOSE")))

(bitfield event-types
          ((:pollin "ZMQ_POLLIN"))
          ((:pollout "ZMQ_POLLOUT"))
          ((:pollerr "ZMQ_POLLERR")))

(bitfield message-options
          ((:noblock "ZMQ_MORE")))

(bitfield send-options
          ((:noblock "ZMQ_DONTWAIT"))
          ((:sndmore "ZMQ_SNDMORE")))

(bitfield transport-events
          ((:event-connected "ZMQ_EVENT_CONNECTED"))
          ((:event-connect-delayed "ZMQ_EVENT_CONNECT_DELAYED"))
          ((:event-connect-retried "ZMQ_EVENT_CONNECT_RETRIED"))
          ((:event-listening "ZMQ_EVENT_LISTENING"))
          ((:event-bind-failed "ZMQ_EVENT_BIND_FAILED"))
          ((:event-accepted "ZMQ_EVENT_ACCEPTED"))
          ((:event-accept-failed "ZMQ_EVENT_ACCEPT_FAILED"))
          ((:event-closed "ZMQ_EVENT_CLOSED"))
          ((:event-close-failed "ZMQ_EVENT_CLOSE_FAILED"))
          ((:event-disconnected "ZMQ_EVENT_DISCONNECTED")))

(constantenum proxy-type
              ((:queue "ZMQ_QUEUE"))
              ((:forwarder "ZMQ_FORWARDER"))
              ((:streamer "ZMQ_STREAMER")))
