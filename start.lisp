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

(reload)
(resume)

(defun ctor-addresses ()
  (let ((section (find-section-by-name (main-image-of (executable-of *process*)) ".ctors")))
    (loop for i from 0 below (length-of section) by 4
       for val = (parse-int (data-bytes-of section) i 4)
       unless (member val '(0 #xFFFFFFFF))
       collect val)))

(defun disasm-at-addr (address size)
  (with-bytes-for-ref (vector offset *memory* size address)
    (dolist (cmd (disassemble-all vector :start offset :end (+ offset size)
                                  :base-address address :errorp nil))
      (when cmd
        (format t "~10X: ~A~%" (x86-instruction-address cmd) (x86-instruction-text cmd))))))

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
                            :executable-hashes '((#x4D90764F . 0)))
             "windows/all.csv" "windows/globals.csv")
  (write-csv (make-instance 'type-context :os-type $linux
                            :executable-hashes '(("fc15065c4d1977ca019c6dad220413d1" . 0)))
             "linux/all.csv" "linux/globals.csv"))

(defun browse-list (start)
  (browse (loop for node = $start.next then $node.next
             while node
             collect $node.item)))
