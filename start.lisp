(ql:quickload :cl-linux-debug.gui)

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

(load "df-code.lisp")

(defvar *process* (start-debug (progn (format t "Enter process ID: ") (read))))
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

(defparameter *csv-stream* nil)
(defparameter *anon-stem* nil)
(defparameter *anon-id* nil)

(defun subname (pname name)
  (if name
      (format nil "~@[~A.~]~A" pname (get-$-field-name name))
      pname))

(defun write-csv-entry (root level pname inc-offset type
                    &key item type-name name)
  (let* ((is-field? (typep type 'data-field))
         (offset (+ inc-offset (if (and is-field? (> level 0))
                                   (effective-offset-of type) 0)))
         (subname (subname pname (or name
                                     (cond ((typep type 'global-type-definition)
                                            (type-name-of type))
                                           (is-field? (name-of type))))))
         (cstring (remove-if (lambda (c) (case c ((#\Newline) t)))
                             (or (comment-string-of type) ""))))
    (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
            root level (format-hex-offset offset)
            (format-hex-offset (effective-size-of type))
            (or type-name (xml:xml-tag-name-string type))
            (or subname "") (or item "") cstring)))

(defgeneric export-csv-rec (type root level pname inc-offset)
  (:method ((type data-item) root level pname inc-offset)
    (write-csv-entry root level pname inc-offset type))
  (:method ((type virtual-compound-item) root level pname inc-offset)
    (if (typep type 'primitive-field)
        (call-next-method)
        (let* ((is-field? (typep type 'data-field))
               (offset (+ inc-offset (if (and is-field? (> level 0))
                                         (effective-offset-of type) 0)))
               (subname (subname pname (if is-field? (name-of type) nil))))
          (when (or (= level 0) (not is-field?) (name-of type))
            (call-next-method))
          (dolist (sub (effective-fields-of type))
            (export-csv-rec sub root (1+ level) subname offset)))))
  (:method ((type global-type-proxy) root level pname inc-offset)
    (write-csv-entry root level pname inc-offset type
                     :type-name (get-$-field-name (type-name-of type))))
  (:method ((type container-item) root level pname inc-offset)
    (let* ((real-elt (effective-contained-item-of type))
           (elt (if (typep real-elt 'pointer)
                    (effective-contained-item-of real-elt)
                    real-elt))
           (anon? (not (or (typep elt 'global-type-proxy)
                           (typep elt 'primitive-field))))
           (elt-name (cond (anon?
                            (format nil "~A::anon~A" *anon-stem* (incf *anon-id*)))
                           ((typep elt 'primitive-field)
                            (xml:xml-tag-name-string elt))
                           (t
                            (get-$-field-name (type-name-of elt))))))
      (write-csv-entry root level pname inc-offset type
                       :item (format nil "~A~A" elt-name (if (eq elt real-elt) "" "*")))
      (when anon?
        (export-csv-rec elt elt-name 0 nil 0))))
  (:method ((type abstract-enum-item) root level pname inc-offset)
    (call-next-method)
    (dolist (field (effective-fields-of type))
      (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
              root (1+ level) "" ""
              (xml:xml-tag-name-string (effective-base-type-of type))
              (aif (name-of field) (get-$-field-name it) "?") (effective-value-of field)
              (remove-if (lambda (c) (case c ((#\Newline) t)))
                         (or (comment-string-of field) ""))))))

(defun export-csv (stream context)
  (let ((*csv-stream* stream))
    (dolist (tn (sort (remove-if #'consp (mapcar #'car *known-types*))
                      (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (let ((type (lookup-type-in-context context tn)))
        (let ((*anon-id* 1)
              (*anon-stem* (get-$-field-name tn)))
          (export-csv-rec type *anon-stem* 0 nil 0))))))

(defun write-csv (context filename)
  (let ((*known-types* (remove-if-not #'consp *known-types* :key #'car))
        (*known-globals* nil)
        (*memory* context))
    (reload)
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (format stream "\"Type\",\"Level\",\"Offset\",\"Size\",\"Field Type\",\"Field Name\",\"Elt Type\",\"Comment\"~%")
      (export-csv stream context))))

(defun make-csv ()
  (write-csv (make-instance 'type-context :os-type $windows) "windows/all.csv")
  (write-csv (make-instance 'type-context :os-type $linux) "linux/all.csv"))
