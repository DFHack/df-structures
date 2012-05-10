(load "start-utils.lisp")

(in-package :work)

(unless *process*
  (setf *process* (start-debug (progn (format t "Enter process ID:~%") (read))))
  (setf *memory* (make-memory-mirror *process* 'object-memory-mirror)))

(if (typep (os-context-of *memory*) 'os-context/windows)
    (progn
      (pushnew 11 (ignored-signals-of *process*))
      (setf (garbage-word-of *memory*) #x33333333))
    (progn
      (setf (garbage-word-of *memory*) #xd2d2d2d2)))

;; disable the known object walk to speed up xml loading
;; use when updating for a new version
;(setf (enumerate-known-objects? *memory*) nil)

(reload)
(resume)

(open-annotations (concatenate 'string *df-version-str* ".lst"))

(browse @global.*)
