(in-package :cl-user)
(defpackage :processpcap
  (:use :cl)
  (:export :output2html-string :get-color-rule-settings :set-color-rule-settings
	   :get-ip-headers :set-ip-headers :set-ip-headers2 :get-filepath
;;	   :seq-to-string :set-filepath))
	   :seq-to-string :pcap-process :set-filepath))

