(in-package :cl-user)
(defpackage :custom-tools
  (:use :cl)
  (:export :debug :*debug-switch* :repeat-string :uniquep
	   :string-null :string-split :string-rm-return :string-rm-newline
	   :markdown-clj :a-pht :json-object-to-list :json-assoc :json-object-generation
	   :string-null :string-split :file-string :stream-string :sequence-to-string
	   :html-string))

