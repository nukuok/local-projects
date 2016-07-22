(in-package :custom-tools)

;;; json related
(defun a-pht (&rest json-object) 
  ;; (a-pht a b) for json object {a:b} , and (list a b) for json object [a,b]
  (alexandria:plist-hash-table
   json-object
   :test #'equal))


(defun json-object-to-list (json-list)
  (when json-list
    (alexandria:hash-table-plist json-list)))

(defun json-assoc (keyword json-list)
  (labels ((iter (keyword list)
	     (unless (null list)
	       (if (equal keyword (car list))
		   (cadr list)
		   (iter keyword (cddr list))))))
    (iter keyword (json-object-to-list json-list))))
