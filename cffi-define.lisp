
;;  ██████╗███████╗███████╗██╗      ██████╗ ███████╗███████╗██╗███╗   ██╗███████╗
;; ██╔════╝██╔════╝██╔════╝██║      ██╔══██╗██╔════╝██╔════╝██║████╗  ██║██╔════╝
;; ██║     █████╗  █████╗  ██║█████╗██║  ██║█████╗  █████╗  ██║██╔██╗ ██║█████╗
;; ██║     ██╔══╝  ██╔══╝  ██║╚════╝██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██╔══╝
;; ╚██████╗██║     ██║     ██║      ██████╔╝███████╗██║     ██║██║ ╚████║███████╗
;;  ╚═════╝╚═╝     ╚═╝     ╚═╝      ╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚══════╝
;; TODO: Add a crash scenario when the value is not number parsable
;; Might want to collect all offenders and then crash at the end
(in-package :cffi-define)

(defmacro defcenum! (name-and-options headers &body enum-list)
  (let ((defines (read-defines headers (mapcar #'car enum-list))))
    `(cffi:defcenum ,name-and-options
       ,@(loop for (c-key key) in enum-list
	       collect (list key (parse-integer (cadr (car (member (string c-key) defines :test 'equal :key 'car)))))))))

(defun read-defines (headers defines)
  (uiop:with-temporary-file (:stream stream :pathname file :prefix "" :suffix ".c")
    (loop for header in headers do (format stream "#include ~a~%" header))
    (format stream "#define XSTR(x) STR(x)~%")
    (format stream "#define STR(x) #x~%")
    (loop for define in defines do (format stream "#pragma message \"~a: \" XSTR(~a)~%" define define))
    (finish-output stream)

    (multiple-value-bind (_ old-name file)
	(rename-file (namestring file) (format nil "~a.c" (namestring file)))
      (declare (ignore _))
      (let* ((output-file (format nil "~a-delete" (namestring file)))
	     (process (uiop:launch-program (list "/usr/bin/env" "gcc" "-shared"
						 "-o" output-file
						 "-Werror" "-fPIC"
						 (namestring file))
					   :output :stream
					   :error-output :stream)))
	(prog1
	    (loop for line in (uiop/stream:slurp-stream-lines (uiop:process-info-error-output process))
		  for result = (cl-ppcre:register-groups-bind (key value)
			      ("#pragma message: (.+): (.+)’" line)
				 (list key value))
		  when result collect result)
	  (rename-file file old-name)
	  (delete-file output-file))))))


;; ┌─┐─┐ ┬┌─┐┌┬┐┌─┐┬  ┌─┐┌─┐
;; ├┤ ┌┴┬┘├─┤│││├─┘│  ├┤ └─┐
;; └─┘┴ └─┴ ┴┴ ┴┴  ┴─┘└─┘└─┘

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
  ;; (AF_DECnet :decnet)
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
