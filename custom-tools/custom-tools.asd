(in-package :cl-user)

(defpackage custom-tools
  (:use :cl :asdf))
(in-package :custom-tools)

(defsystem custom-tools
  :description "custom-tools"
  :version "0.1"
  :author "huang.yangyang"
  :license "GPL"
  :components ((:file "package")
               (:module "src"
                :components
                (;(:file "string-tools")
		 (:file "comment-extract")
		 (:file "json-related")
		 (:file "custom-tools"))))
  :in-order-to ((test-op (load-op custom-tools))))
