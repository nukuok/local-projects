(in-package :cl-user)
(defpackage :custom-tools
  (:use :cl)
  (:export :debug :*debug-switch* :repeat-string :uniquep
	   :string-null :string-split :string-rm-return :string-rm-newline))

