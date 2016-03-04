(in-package :processpcap)

(defvar *filepath* nil)
(setf *filepath* "C:/Users/Administrator/Desktop/")

(defun get-filepath ()
  (format nil "~a" *filepath*))

(defun set-filepath (string)
  (setf *filepath* (string-trim #(#\space #\return) string)))
