(in-package :custom-tools)

(defun string-null (mame)
  (equal mame ""))

(defun string-split (string char &optional result)
  (let ((char-position (position char string)))
    (cond ((string-null string) (reverse result))
	  (char-position (string-split (subseq string (+ 1 char-position))
		      char (cons (subseq string 0 char-position) result)))
	  (t (string-split "" char (cons string result))))))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun stream-string (stream)
  (let ((result (make-sequence 'string (length stream))))
    (read-sequence result stream)
    result))

(defmacro sequence-to-string (seq)
  `(concatenate 'string (map 'sequence #'code-char ,seq)))
