#+quicklisp
(ql:quickload :cl-linux-debug.gui)
#-quicklisp
(asdf:load-system :cl-linux-debug.gui)

(defpackage :work 
  (:use :common-lisp
        :alexandria :anaphora
        :cl-linux-debug.code-info
        :cl-linux-debug
        :cl-linux-debug.field-names
        :cl-linux-debug.data-defs
        :cl-linux-debug.data-info
        :cl-linux-debug.gui)
  (:import-from :cl-linux-debug.code-info #:parse-int))

(in-package :work)

(setf *print-length* 50)

;; Best effort to alter the root value for all threads
(enable-gui-debugger-hook)
(bt:make-thread (lambda () (enable-gui-debugger-hook)))

(load "df-code.lisp")

(defvar *process* (start-debug (progn (format t "Enter process ID:~%") (read))))
(defvar *memory* (make-memory-mirror *process* 'object-memory-mirror))

(define-symbol-macro global *memory*)

(defun reload ()
  (dolist (i (directory (merge-pathnames #P"*.xml" #.*load-truename*)))
    (register-data-definition *memory* i))
  (let ((subdir (case (os-type-of *memory*)
                  ($windows #P"windows/*.xml")
                  (otherwise #P"linux/*.xml"))))
    (dolist (i (directory (merge-pathnames subdir #.*load-truename*)))
      (register-data-definition *memory* i)))
  (check-refresh-context *memory*))

(defun refresh ()
  (refresh-memory-mirror *memory*))

(defun browse (obj)
  (browse-object-in-new-window *memory* obj))

(defun browse-addr (addr &optional (tname 'padding))
  (browse (make-ad-hoc-memory-ref *memory* addr (make-instance tname) :parent :addr)))

(defun resume ()
  (call-debug-task 'resume-all-threads *process*))

(defun suspend ()
  (call-debug-task 'stop-all-threads *process*))

(defun find-str (string &key any-prefix? any-suffix?)
  (browse (find-stl-strings *memory* string
                            :any-prefix? any-prefix?
                            :any-suffix? any-suffix?)))

(if (eq (os-type-of *memory*) $windows)
    (progn
      (pushnew 11 (ignored-signals-of *process*))
      (setf (garbage-word-of *memory*) #x33333333))
    (progn
      (setf (garbage-word-of *memory*) #xd2d2d2d2)))

;; disable the known object walk to speed up xml loading
;; use when updating for a new version
(setf (enumerate-known-objects? *memory*) nil)

(reload)
(resume)

(open-annotations "v0.34.02.lst")

(defun write-csv (context filename gfilename)
  (let ((*known-types* (remove-if-not #'consp *known-types* :key #'car))
        (*known-globals* nil)
        (*memory* context))
    (reload)
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (export-csv stream context))
    (with-open-file (stream gfilename :direction :output :if-exists :supersede)
      (export-csv stream context :globals? t))))

(defun make-csv ()
  (write-csv (make-instance 'type-context :os-type $windows
                            :executable-hashes '((#x4F3F88BC . 0)))
             "windows/all.csv" "windows/globals.csv")
  (write-csv (make-instance 'type-context :os-type $linux
                            :executable-hashes '(("f4fc83475f9fdaa645f3217c57458235" . 0)))
             "linux/all.csv" "linux/globals.csv"))

(defun browse-list (start)
  (browse (loop for node = $start.next then $node.next
             while node
             collect $node.item)))

(load "disasm.lisp")

(defun reset-state-annotation ()
  (annotate-all *memory* :status :unchecked
                :filter @$(and (typep $ '(or struct-compound-item enum-field))
                               (name-of $))
                :namespace nil))
