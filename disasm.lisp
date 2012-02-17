(in-package :work)

(defun ctor-addresses (address size)
  (with-bytes-for-ref (vector offset *memory* size address)
    (loop for i from 0 below size by 4
       for val = (parse-int vector (+ offset i) 4)
       unless (member val '(0 #xFFFFFFFF))
       collect val)))

(defun disasm-at-addr (address size)
  (with-bytes-for-ref (vector offset *memory* size address)
    (dolist (cmd (disassemble-all vector :start offset :end (+ offset size)
                                  :base-address address :errorp nil))
      (when cmd
        (format t "~10X: ~A~%" (x86-instruction-address cmd) (x86-instruction-text cmd))))))

(defun collect-ctor-globals (address)
  (with-bytes-for-ref (vector offset *memory* 16 address)
    (let ((executable (executable-of *process*))
          (globals nil)
          (dtor-addr nil)
          (obj-addr nil)
          (ctors nil)
          (atexit-addrs
           (mapcar #'start-address-of
                   (remove-if-not (lambda (x) (typep (origin-of x) 'executable-region-plt-entry))
                                  (find-regions-by-name (executable-of *process*) "__cxa_atexit")))))
      (labels ((describe-function (addr)
                 (format nil "0x~X~@[ [~A]~]" addr
                         (awhen (find-region-by-address executable addr)
                           (symbol-name-of it))))
               (is-atexit? (addr)
                 (member addr atexit-addrs)))
        (disassemble-iter (cmd vector :start offset :base-address address :errorp nil)
          (case (x86-instruction-mnemonic cmd)
            (:ret (return))
            (:call (let ((addr (x86-instruction-addr-value cmd)))
                     (if (is-atexit? addr)
                         (let ((cmstr (format nil "~{ctor ~A; ~}dtor ~A"
                                              (mapcar #'describe-function ctors)
                                              (describe-function dtor-addr))))
                           (when globals
                             (let ((size (- obj-addr (offset-of (first globals)))))
                               (when (> size 0)
                                 (setf (size-of (first globals)) size))))
                           (push (make-instance 'global-object
                                                :name (cl-linux-debug.data-info::get-$-field
                                                       (format nil "obj_~X" obj-addr))
                                                :offset obj-addr
                                                :comment (make-instance 'comment :content cmstr))
                                 globals)
                           (setf dtor-addr nil obj-addr nil ctors nil))
                         (push addr ctors))))
            (:mov (let ((arg1 (x86-instruction-argument1 cmd))
                        (arg2 (x86-instruction-argument2 cmd)))
                    (when (and (typep arg1 'x86-argument-memory)
                               (typep arg2 'x86-argument-constant)
                               (eq (x86-argument-memory-base-reg arg1) :esp)
                               (null (x86-argument-memory-index-reg arg1)))
                      (case (x86-argument-memory-displacement arg1)
                        (0 (setf dtor-addr (x86-instruction-immediate cmd)))
                        (4 (setf obj-addr (x86-instruction-immediate cmd)))))))))
        (nreverse globals)))))

(defun list-globals/linux ()
  (let ((section (find-section-by-name (main-image-of (executable-of *process*)) ".ctors")))
    (loop for addr in (ctor-addresses (start-address-of section) (length-of section))
       nconc (collect-ctor-globals addr))))

