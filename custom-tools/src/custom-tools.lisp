(in-package :custom-tools)

(defun prune (test tree)
 (labels ((rec (tree acc)
           (cond
            ((null tree) (nreverse acc))
            ((consp (car tree))
             (rec (cdr tree)
                  (cons (rec (car tree) nil) acc)))
            (t (rec (cdr tree)
                    (if (funcall test (car tree))
                     acc
                     (cons (car tree) acc)))))))
   (rec tree nil)))

(defun process-tree (process tree)
 (labels ((rec (tree acc)
           (cond
            ((null tree) (nreverse acc))
            ((consp (car tree))
             (rec (cdr tree)
                  (cons (rec (car tree) nil) acc)))
            (t (rec (cdr tree)
		    (cons (funcall process (car tree)) acc))))))
   (rec tree nil)))


(defvar *debug-switch*)
(setf *debug-switch* nil)
  
(defmacro debug (message &rest parameters)
  `(when *debug-switch*
     (format t "~%~A~%" ,message)
     (loop for x in (list ,@parameters) do
	  (print x))))

(defmacro repeat-string (string times)
  `(concatenate 'string
		,@(loop for x below times collect string)))


(defun uniquep (element list)
  (= (- (length list) 1)
     (length (remove element list :test #'equal))))

(defun last-1 (mame)
  (car (last mame)))
