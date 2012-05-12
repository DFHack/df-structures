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

;; Load the symbol tables for os type detection
(load-data-definition (merge-pathnames #P"symbols.xml" #.*load-truename*))

(defvar *process* nil)
(defvar *memory* nil)

(define-symbol-macro global *memory*)

(defun reload ()
  (dolist (i (directory (merge-pathnames #P"*.xml" #.*load-truename*)))
    (register-data-definition *memory* i))
  (let ((subdir (typecase (os-context-of *memory*)
                  (os-context/windows #P"windows/*.xml")
                  (t #P"linux/*.xml"))))
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

(load "version.lisp")

(defun write-csv (context filename gfilename)
  (let ((*known-types* (remove-if-not #'consp *known-types* :key #'car))
        (*known-globals* nil)
        (*memory* context))
    (check-refresh-context context)
    (reload)
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (export-csv stream context))
    (with-open-file (stream gfilename :direction :output :if-exists :supersede)
      (export-csv stream context :globals? t))))

(defun make-csv ()
  (write-csv (make-instance 'type-context ;:os-type $windows
                            :executable-hashes (list (cons *windows-timestamp* 0)))
             "windows/all.csv" "windows/globals.csv")
  (write-csv (make-instance 'type-context ;:os-type $linux
                            :executable-hashes (list (cons *linux-hash* 0)))
             "linux/all.csv" "linux/globals.csv"))

(defun browse-list (start)
  (browse (loop for node = $start.next then $node.next
             while node
             collect $node.item)))

(load "disasm.lisp")

(defun reset-state-annotation (&key mark-substructs?)
  (annotate-all *memory* :status :unchecked
                :filter @$(if mark-substructs?
                              (and (typep $ '(or struct-compound-item enum-field))
                                   (or (name-of $) (is-contained-item? $)))
                              (or (and (typep $ 'enum-field)
                                       (or (name-of $) (is-contained-item? $)))
                                  (and (typep $ 'struct-compound-item)
                                       (is-contained-item? $))
                                  (typep $ 'global-type-definition)))
                :namespace nil)
  (save-annotations))

(defun check-struct-sizes (&key annotate?)
  (unless (typep (os-context-of *memory*) 'os-context/windows)
    (error "Only the WINE version has precise heap chunk sizes."))
  (multiple-value-bind (correct faulty)
      (verify-object-sizes *memory*)
    (browse faulty)
    (when annotate?
      (dolist (ref correct)
        (let ((type (memory-object-ref-type ref)))
          (when (eq (type-annotation type :status) :unchecked)
            (setf (type-annotation type :status) :aligned))))
      (save-annotations))))

(defun browse-dataseg ()
  (let ((img (main-image-of (executable-of *process*))))
    (browse-addr (start-address-of (find-section-by-name img ".data")))))

(defun object-stats ()
  (malloc-object-stats *memory*))

(defun prompt (fmt &rest args)
  (apply #'format t fmt args)
  (finish-output *standard-output*)
  (with-simple-restart (continue "Ignore parse error")
    (read-from-string (concatenate 'string "(" (read-line) ")"))))

(defun find-changes ()
  (refresh)
  (prompt "Make the first change and press enter...")
  (let ((info (begin-find-changes *memory*))
        (found nil))
    (loop for cmd = (prompt "~S~%Enter 0-~A/+ delta/new/done/abort: "
                            info (1- (length (value-sets-of info))))
       do (with-simple-restart (continue "Ignore error")
            (if (numberp (first cmd))
                ;; Return to a previous state
                (update-find-changes info :state (first cmd))
                (case (first cmd)
                  ;; Known increment to a previous value
                  (+ (update-find-changes info
                                          :increment (or (second cmd) 1)
                                          :state (third cmd)))
                  ;; Completely new value
                  ((n new) (update-find-changes info))
                  ;; Quit
                  (abort
                   (return))
                  (done
                   (setf found (get-found-changes info))
                   (return))))))
    (refresh)
    (if found
        (browse found)
        (format t "No changes found.~%"))))
