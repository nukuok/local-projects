(in-package :custom-tools)

(defun string-equal-from-head (string1 string2)
  (let ((string1-length (length string1))
	(string2-length (length string2)))
    (cond ((< string1-length string2-length)
	   (if (string-equal string1 string2 :end2 string1-length) string1))
	  ((> string1-length string2-length)
	   (if (string-equal string1 string2 :end1 string2-length) string2))
	  ((= string1-length string2-length)
	   (if (string-equal string1 string2) string1)))))

(defun string-equal-from-tail (string1 string2)
  (let ((string1-length (length string1))
	(string2-length (length string2)))
    (cond ((< string1-length string2-length)
	   (if (string-equal string1 string2 :start2 (- string2-length string1-length)) string1))
	  ((> string1-length string2-length)
	   (if (string-equal string1 string2 :start1 (- string1-length string2-length)) string2))
	  ((= string1-length string2-length)
	   (if (string-equal string1 string2) string1)))))


(defun comment-begin-p (string &key
				 (judge-function (lambda (x) (string-equal-from-head "\"\"\"" x)))
				 (preprocess-function (lambda (x) (string-trim " " x))))
  (funcall judge-function (funcall preprocess-function string)))

(defun comment-end-p (string &key
				 (judge-function (lambda (x) (string-equal-from-tail "\"\"\"" x)))
				 (preprocess-function (lambda (x) (string-trim " " x))))
  (funcall judge-function (funcall preprocess-function string)))

(defun markdown-begin-p (string)
  (comment-begin-p string :judge-function (lambda (x) (and (string-equal-from-head "@cond" x)
							   (string-equal-from-tail "markdown" x)))))

(defun markdown-end-p (string)
  (comment-begin-p string :judge-function (lambda (x) (string-equal-from-tail "@endcond" x))))

(defun markdown-clj (adjust-line); &key status-changed-word)
  (let ((in-comment nil)
	(in-markdown nil))
	;(scw (or status-changed-word "mame")))
    (lambda (line)
      (progn
	(format t "in-comment: ~A, in-markdown: ~A" in-comment in-markdown)
	(cond
	  ((and (not in-comment) (not in-markdown) (comment-begin-p line))
	   (setq in-comment 1) nil)
	  ((and in-comment (not in-markdown) (markdown-begin-p line))
	   (setq in-markdown 1) nil)
	  ((and in-comment in-markdown (markdown-end-p line))
	   (setq in-markdown nil))
	  ((and in-comment (comment-end-p line))
	   (setq in-comment nil) (setq in-markdown nil))
	  ((and in-comment in-markdown)
	   (funcall adjust-line line)))))))
