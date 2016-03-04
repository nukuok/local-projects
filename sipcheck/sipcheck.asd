(in-package :cl-user)
(ql:quickload :cl-ppcre)
(ql:quickload :cxml)
(ql:quickload :custom-tools)

(defpackage sipcheck
  (:use :cl :asdf :cl-ppcre :cxml :custom-tools))
(in-package :sipcheck)

(defsystem sipcheck
  :description "sipcheck"
  :version "0.1"
  :author "huang.yangyang"
  :license "GPL"
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "check")
		 (:file "compare-message")
		 (:file "data-extract2"))))
  :in-order-to ((test-op (load-op sipcheck))))
