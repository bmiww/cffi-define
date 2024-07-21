
;;  ██████╗███████╗███████╗██╗      ██████╗ ███████╗███████╗██╗███╗   ██╗███████╗
;; ██╔════╝██╔════╝██╔════╝██║      ██╔══██╗██╔════╝██╔════╝██║████╗  ██║██╔════╝
;; ██║     █████╗  █████╗  ██║█████╗██║  ██║█████╗  █████╗  ██║██╔██╗ ██║█████╗
;; ██║     ██╔══╝  ██╔══╝  ██║╚════╝██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██╔══╝
;; ╚██████╗██║     ██║     ██║      ██████╔╝███████╗██║     ██║██║ ╚████║███████╗
;;  ╚═════╝╚═╝     ╚═╝     ╚═╝      ╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚══════╝
(asdf:defsystem #:cffi-define
  :serial t
  :description "A wrapper around defcenum to read C defines and compile a cffi defcenum form."
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.1"
  :depends-on (#:cffi #:cl-ppcre)
  :components ((:file "package")
	       (:file "cffi-define")))
