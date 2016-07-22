(in-package :custom-tools)

(defun read-from-stream-wait (stream listen-ng-times result)
  (cond ((< listen-ng-times 0) (coerce result 'string))
	((listen stream)
	 (read-from-stream-wait stream listen-ng-times
				(append result (list (read-char stream)))))
	(t (sleep 0.1)
	   (read-from-stream-wait stream (- listen-ng-times 1) result))))

(defclass plink ()
  ((process :accessor plink-process :initarg :pp)
   (instream :accessor plink-in :initarg :pi)
   (outstream :accessor plink-out :initarg :po)))

(defmethod new-plink-connection ((command list))
  (let ((mame-process
	 (ccl:run-program "plink" command :input :stream :output :stream :wait nil :sharing :lock)))
    (make-instance 'plink
		   :pp mame-process
		   :pi (ccl:external-process-input-stream mame-process)
		   :po (ccl:external-process-output-stream mame-process))))

(defmethod plink-status ((instance plink))
  (ccl:external-process-status (plink-process instance)))

(defmethod plink-get-output ((instance plink))
  (read-from-stream-wait (plink-out instance) 10 nil))

(defmethod plink-command-input ((instance plink) (command string))
  (progn (format (plink-in instance) "~A~%" command)
	 (finish-output (plink-in instance))))

