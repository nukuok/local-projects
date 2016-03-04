(in-package :cl-user)
(ql:quickload :cl-ppcre)

(defpackage processpcap
  (:use :cl :asdf :cl-ppcre))
(in-package :processpcap)

(defsystem processpcap
  :description "processpcap"
  :version "0.1"
  :author "huang.yangyang"
  :license "GPL"
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "output-to-var")
		 (:file "color-rule")
		 (:file "ip-header")
		 (:file "filepath")
		 (:file "settings")
		 (:file "rtp")
		 (:file "test-05-2html")
		 (:file "test-05-udp")
		 (:file "tools"))))
  :in-order-to ((test-op (load-op processpcap))))
