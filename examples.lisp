
;; ███████╗██╗  ██╗ █████╗ ███╗   ███╗██████╗ ██╗     ███████╗███████╗
;; ██╔════╝╚██╗██╔╝██╔══██╗████╗ ████║██╔══██╗██║     ██╔════╝██╔════╝
;; █████╗   ╚███╔╝ ███████║██╔████╔██║██████╔╝██║     █████╗  ███████╗
;; ██╔══╝   ██╔██╗ ██╔══██║██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝  ╚════██║
;; ███████╗██╔╝ ██╗██║  ██║██║ ╚═╝ ██║██║     ███████╗███████╗███████║
;; ╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝╚══════╝
(in-package :cffi-define)

;; Reads the enum values for socket address families from the linux kernel header
;; As a prime example: on the system i was writing this on - the DECnet address family was not defined
;; https://github.com/torvalds/linux/blob/master/include/linux/socket.h
(defcenum! address-families
    ("<sys/socket.h>")
  (AF_UNSPEC :unspec)
  (AF_UNIX :unix)
  (AF_LOCAL :local)
  (AF_INET :inet)
  (AF_AX25 :ax25)
  (AF_IPX :ipx)
  (AF_APPLETALK :appletalk)
  (AF_NETROM :netrom)
  (AF_BRIDGE :bridge)
  (AF_ATMPVC :atmpvc)
  (AF_X25 :x25)
  (AF_INET6 :inet6)
  (AF_ROSE :rose)
  (AF_DECnet :decnet)
  (AF_NETBEUI :netbeui)
  (AF_SECURITY :security)
  (AF_KEY :key)
  (AF_NETLINK :netlink)
  (AF_ROUTE :route)
  (AF_PACKET :packet)
  (AF_ASH :ash)
  (AF_ECONET :econet)
  (AF_ATMSVC :atmsvc)
  (AF_RDS :rds)
  (AF_SNA :sna)
  (AF_IRDA :irda)
  (AF_PPPOX :pppox)
  (AF_WANPIPE :wanpipe)
  (AF_LLC :llc)
  (AF_IB :ib)
  (AF_MPLS :mpls)
  (AF_CAN :can)
  (AF_TIPC :tipc)
  (AF_BLUETOOTH :bluetooth)
  (AF_IUCV :iucv)
  (AF_RXRPC :rxrpc)
  (AF_ISDN :isdn)
  (AF_PHONET :phonet)
  (AF_IEEE802154 :ieee802154)
  (AF_CAIF :caif)
  (AF_ALG :alg)
  (AF_NFC :nfc)
  (AF_VSOCK :vsock)
  (AF_KCM :kcm)
  (AF_QIPCRTR :qipcrtr)
  (AF_SMC :smc)
  (AF_XDP :xdp)
  (AF_MCTP :mctp)
  (AF_MAX :max))
