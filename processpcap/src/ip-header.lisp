(in-package :processpcap)

(defvar *ip-header-list-v4*)
(setf *ip-header-list-v4*
      '((222 231 228 186)
	(222 231 228 139)
	(222 231 228 11)
	(222 231 228 58)))

(defvar *ip-header-list-v4-2*)
(setf *ip-header-list-v4-2*
      '((222 231 228 187)
	(222 231 228 140)
	(222 231 228 12)
	(222 231 228 59)))

(defvar *ip-header-list-v4-3*)
(setf *ip-header-list-v4-3*
      '((10 20 91 5)
	()
	()
	()))

(defvar *ip-header-list-v6*)
(setf *ip-header-list-v6*
      '((32 1 167 255 33 0 0 13 0 0 0 0 0 0 240 1)
	(32 1 167 255 33 0 0 13 0 0 0 0 0 0 0 17)
	(32 1 167 255 33 0 0 12 0 0 0 0 0 0 0 17)
	(32 1 167 255 33 0 0 12 0 0 0 0 0 0 240 1)))

(defvar *ip-header-list-v6-2*)
(setf *ip-header-list-v6-2*
      '((32 0 0 2 0 0 0 0 0 0 0 0 0 145 0 5)
	(32 1 167 255 33 0 0 13 0 0 0 0 0 0 0 18)
	()
	()))
	

(defvar *ip-header-list* nil)
(setf *ip-header-list*
      (list
       *ip-header-list-v4*
       *ip-header-list-v4-2*
       *ip-header-list-v4-3*
       *ip-header-list-v6*
       *ip-header-list-v6-2*
       ))

(defun fmt-ip4 (list)
  (format nil "~d.~d.~d.~d~%"
	  (nth 0 list)
	  (nth 1 list)
	  (nth 2 list)
	  (nth 3 list)))

(defun fmt-ip6 (list)
  (concatenate 'string
	       (format nil "~{~2,'0x~2,'0x:~}" (subseq list 0 14))
	       (format nil "~{~2,'0x~2,'0x~}~%" (subseq list 14 16))))

(defun get-ip-headers (part)
  (apply #'concatenate
	   (cons 'string
		 (loop for x in *ip-header-list* collect
		      (let ((temp-ip (nth part x)))
			(let ((ll (length temp-ip)))
			  (cond ((= ll 4) (fmt-ip4 temp-ip))
				((= ll 16) (fmt-ip6 temp-ip))
				(t nil))))))))
(defun set-ip-headers (p1 p2 p3 p4)
  (let ((mame (loop for ii from 0
		 as cp1 = (nth ii (ipstrings-to-list p1))
		 as cp2 = (nth ii (ipstrings-to-list p2))
		 as cp3 = (nth ii (ipstrings-to-list p3))
		 as cp4 = (nth ii (ipstrings-to-list p4))
		 while (or cp1 cp2 cp3 cp4) collect
		   (list cp1 cp2 cp3 cp4))))
    (setf *ip-header-list* mame)))

(defun max-member-length (&rest members)
  (apply #'max (mapcar #'length members)))

(defun set-ip-headers2 (p1 p2 p3 p4)
  (let ((mml (max-member-length p1 p2 p3 p4)))
    (let ((mame (loop for ii from 0 to mml
		   as cp1 = (nth ii (ipstrings-to-list p1))
		   as cp2 = (nth ii (ipstrings-to-list p2))
		   as cp3 = (nth ii (ipstrings-to-list p3))
		   as cp4 = (nth ii (ipstrings-to-list p4))
		   collect (list cp1 cp2 cp3 cp4))))
    (setf *ip-header-list* mame))))


(defun ipstrings-to-list (string)
  (let ((instream (make-string-input-stream string)))
    (labels ((acc (in result)
	       (let ((current-line
		      (string-trim #(#\space #\return)
				   (read-line in nil "eofeofeof"))))
		 (if (equal current-line "eofeofeof")
		     (reverse result)
		     (acc in (cons (ipstring-to-dig current-line) result))))))
      (acc instream nil))))

(defun null-string (string)
  (string-equal "" string))

(defun ipstring-to-dig (string)
  (cond ((null-string (string-trim "01234567890. " string))
		      (ipstring-to-dig-ipv4 string))
	((null-string (string-trim "01234567890ABCDEFabcdef: " string))
		      (ipstring-to-dig-ipv6 string))
	(t nil)))

(defun ipstring-to-dig-ipv4 (string)
  (let ((temp-result (split-by string #\.)))
    (when (= (length temp-result) 4) (mapcar #'parse-integer temp-result))))

(defun ipstring-to-dig-ipv6 (string)
  (let* ((temp-result (split-by string #\:))
	 (ll-result (length temp-result)))
    (if (= ll-result 8)
	(loop for x in temp-result append
	     (ipstring-section-to-dig-ipv6 x))
	(let ((double-colon-position
	       (position "" temp-result :test #'string-equal)))
	  (when double-colon-position
	    (let
		((mame
		  (append (loop for ii below double-colon-position
			     collect (nth ii temp-result))
			  (loop for ii below (- 8 ll-result) collect "0000")
			  (loop for ii from (+ 1 double-colon-position) below
			     ll-result collect (nth ii temp-result)))))
	      (loop for x in mame append
		   (ipstring-section-to-dig-ipv6 x))))))))

(defun ipstring-section-to-dig-ipv6 (string)
  (let ((ll-string (length string)))
    (cond ((= ll-string 1) (list 0 (parse-integer string :radix 16)))
	  ((= ll-string 2) (list 0 (parse-integer string :radix 16)))
	  ((= ll-string 3) (list (parse-integer (subseq string 0 1) :radix 16)
				 (parse-integer (subseq string 1) :radix 16)))
	  ((= ll-string 4) (list (parse-integer (subseq string 0 2) :radix 16)
				 (parse-integer (subseq string 2) :radix 16)))
	  (t nil))))

