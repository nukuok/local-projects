(in-package :custom-tools)

(defun html-string (string &optional color)
  (cond
    (color (with-html-output-to-string (a) (:font :color color (fmt "~a" string))))
    (t (with-html-output-to-string (a) (fmt "~a" string)))))

