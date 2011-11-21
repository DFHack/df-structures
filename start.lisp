(ql:quickload :cl-linux-debug.gui)

(defpackage :work 
  (:use :common-lisp
        :alexandria :anaphora
        :cl-linux-debug.code-info
        :cl-linux-debug
        :cl-linux-debug.field-names
        :cl-linux-debug.data-defs
        :cl-linux-debug.data-info
        :cl-linux-debug.gui))

(in-package :work)

(defvar *process* (start-debug (progn (format t "Enter process ID: ") (read))))
(defvar *memory* (make-memory-mirror *process* 'object-memory-mirror))

(define-symbol-macro global *memory*)

(defun reload ()
  (dolist (i (directory (merge-pathnames #P"*.xml" #.*load-truename*)))
    (load-data-definition i))
  (check-refresh-context *memory*))

(defun refresh ()
  (refresh-memory-mirror *memory*))

(defun browse (obj)
  (browse-object-in-new-window *memory* obj))
