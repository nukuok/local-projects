(in-package :processpcap)

(defvar *color-rule* nil)
(setf *color-rule*
      '(("cpc=[a-zA-Z]*" "orange")
	("tag=[a-zA-Z0-9\.\+]*" "red")
	;;("sip:[\\[\\]a-fA-F0-9:.]*:5060" "purple")
	("o=- [0-9]* [0-9]*" "red")
	("@[a-zA-Z.]+" "blue")
	("audio [0-9]*" "red")
	("P-Early-Media: supported" "green")
	("[0-9]+[.][0-9.]+[.][0-9.]+[.][0-9]+" "red")
	;;("IP[46] [\\[\\]a-fA-F0-9:.]*" "red")
	("This-is-a-test-call:" "green")
	("This-is-a-message:" "red")))


(defun get-color-rule-settings ()
  (format t "~{~{'~a'  ~}~%~}" *color-rule*))

(defun set-color-rule-settings (string)
  (let ((instream (make-string-input-stream string)))
    (labels ((acc (instream result)
	       (let ((a-line (read-line instream nil "eofeofeof")))
		 (if (string-equal "eofeofeof" a-line)
		     (reverse result)
		     (acc instream
			  (cons (remove-if
				 (lambda (x) (equal "" (string-trim " " x)))
				 (split-by-one-quote
				  (string-trim
				   #(#\space #\return)
				   a-line)))
				 result))))))
      (setf *color-rule* (remove-if #'null (acc instream nil))))))


