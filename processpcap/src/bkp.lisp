(in-package :processpcap)

(defun is-not-arrows-nor-nil (mame)
  (not (or (null mame)
	   (when (> (length mame) 9)
	     (let ((part (subseq mame 0 6)))
	       (or (equal part "-right")
		   (equal part "-left-")))))))
	       
	   
(defun output-item-with-maru (left-messages right-messages outstream index cc)
  (when (> cc 0)
    (let ((mame1 (substitute-for-html-string (cadr (assoc index left-messages))))
	  (mame2 (substitute-for-html-string (cadr (assoc index right-messages)))))
      (output-tr outstream
		 (write-to-string index)
		 mame1
		 (when (is-not-arrows-nor-nil mame1) "ü")
		 mame2
		 (when (is-not-arrows-nor-nil mame2) "ü"))
      (output-item-with-maru left-messages right-messages outstream (+ index 1) (- cc 1)))))

(defun output2file-with-maru (filename)
  (let ((result (remove-messages-duplicates (pcap-process filename)))
	(outfile (concatenate 'string filename ".html")))
    (message-list-initial)
    (process-result result 0)
    (with-open-file (outstream outfile :direction :output
			       :if-does-not-exist :create
			       :if-exists :supersede)
      (output-html-header outstream)
      (output-tr outstream
		 "ngn"
		 nil
		 "sbc"
		 nil
		 "carrier")
      (output-tr outstream
		 (list "ip->" "right")
		 (ip-list-2html (cdr (assoc 1 *ip-list*)))
		 (list "ip->" "right")
		 (ip-list-2html (cdr (assoc 3 *ip-list*)))
		 nil)
      (output-tr outstream
		 nil
		 (list (ip-list-2html (cdr (assoc 2 *ip-list*))) "right")
		 "->ip"
		 (list (ip-list-2html (cdr (assoc 4 *ip-list*))) "right")
		 "->ip")
      (output-item-with-maru (reverse *left-messages*)
		   (reverse *right-messages*)
		   outstream
		   1
		   (max (or (caar *left-messages*) 0)
			(or (caar *right-messages*) 0)))
      ;;(print *left-messages*)
      (output-html-tail outstream))))

;;(setf processpcap::*color-rule* nil)

(defun run-marumaru ()
  (loop for x in (directory "*.pcap*") do
     (format t "Processing-file: ~A~%" (namestring x))
       (output2file-with-maru (namestring x))))


