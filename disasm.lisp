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

(defun get-function-addrs (name)
  (mapcar #'start-address-of
          (remove-if-not (lambda (x &aux (org (origin-of x)))
                           (or (typep org 'executable-region-plt-entry)
                               (typep org 'executable-region-function)))
                         (find-regions-by-name (executable-of *process*) name))))

(defun describe-function (addr)
  (format nil "0x~X~@[ [~A]~]" addr
          (awhen (find-region-by-address (executable-of *process*) addr)
            (symbol-name-of it))))

(defun is-stack-var? (arg)
  (when (x86-argument-matches? arg :base-reg :esp :index-reg nil)
    (x86-argument-memory-displacement arg)))

(defun collect-ctor-globals (address)
  (with-bytes-for-ref (vector offset *memory* 16 address)
    (let ((globals nil)
          (dtor-addr nil)
          (obj-addr nil)
          (ctors nil)
          (atexit-addrs (get-function-addrs "__cxa_atexit")))
      (labels ((is-atexit? (addr)
                 (member addr atexit-addrs)))
        (disassemble-iter (cmd vector :start offset :base-address address :errorp nil)
          (case (x86-instruction-mnemonic cmd)
            (:ret (return))
            (:call (let ((addr (x86-instruction-addr-value cmd)))
                     (if (is-atexit? addr)
                         (let ((cmstr (format nil "~{ctor ~A; ~}dtor ~A"
                                              (mapcar #'describe-function ctors)
                                              (describe-function dtor-addr))))
                           (when (and globals (null (size-of (first globals))))
                             (let ((size (- obj-addr (offset-of (first globals)))))
                               (when (> size 0)
                                 (setf (size-of (first globals)) size))))
                           (when obj-addr
                             (push (make-instance 'global-object
                                                  :name (cl-linux-debug.data-info::get-$-field
                                                         (format nil "obj_~X" obj-addr))
                                                  :offset obj-addr
                                                  :comment (make-instance 'comment :content cmstr))
                                   globals))
                           (setf dtor-addr nil obj-addr nil ctors nil))
                         (push addr ctors))))
            (:mov (when (typep (x86-instruction-argument2 cmd) 'x86-argument-constant)
                    (case (is-stack-var? (x86-instruction-argument1 cmd))
                      (0 (setf dtor-addr (x86-instruction-immediate cmd)))
                      (4 (setf obj-addr (x86-instruction-immediate cmd))))))))
        (nreverse globals)))))

(defun list-globals/linux ()
  (let ((section (find-section-by-name (main-image-of (executable-of *process*)) ".ctors")))
    (loop for addr in (ctor-addresses (start-address-of section) (length-of section))
       nconc (collect-ctor-globals addr))))

(defun scan-linux-destructor (address)
  (with-bytes-for-ref (vector offset *memory* 16 address)
    (let* ((region (find-region-by-address (executable-of *process*) address))
           (length nil)
           (cur-cfa 4)
           (push-depth 0)
           (unwinds nil)
           (reg-state nil)
           (stack-top nil)
           (del-one (get-function-addrs "_ZdlPv"))
           (del-arr (get-function-addrs "_ZdaPv"))
           (del-str (get-function-addrs "_ZNSsD1Ev")))
      (when region
        (setf vector (data-bytes-of region)
              offset (start-offset-of region)
              address (start-address-of region)
              length (or (length-of region) length)
              unwinds (region-unwind-table (origin-of region))))
      (labels ((get-callee-name (addr)
                 (cond ((member addr del-one)
                        "delete")
                       ((member addr del-arr)
                        "delete[]")
                       ((member addr del-str)
                        "~string")
                       (t
                        (describe-function addr))))
               (emit-call (cmd name)
                 (format t "~X: call ~A: ~A~%"
                         (x86-instruction-address cmd)
                         name stack-top))
               (get-reg (reg)
                 (if reg
                     (or (assoc-value reg-state reg) "?")
                     nil))
               (invalidate-reg (arg)
                 (when (and (typep arg 'x86-argument-register)
                            (x86-argument-access-write? arg))
                   (setf (assoc-value reg-state (x86-argument-register-id arg)) nil)))
               (invalidate (cmd)
                 (invalidate-reg (x86-instruction-argument1 cmd))
                 (invalidate-reg (x86-instruction-argument2 cmd))))
        (disassemble-iter (cmd vector
                               :start offset :base-address address
                               :end (if length (+ offset length))
                               :errorp nil)
          (let ((opcode (x86-instruction-mnemonic cmd))
                (arg1 (x86-instruction-argument1 cmd))
                (arg2 (x86-instruction-argument2 cmd))
                (addr (x86-instruction-addr-value cmd))
                (imm (x86-instruction-immediate cmd)))
            (case opcode
              (:ret (return))
              ;; Track stack frame changes
              (:push
               (incf cur-cfa 4)
               (incf push-depth))
              (:pop
               (decf cur-cfa 4)
               (decf push-depth)
               (setf stack-top nil)
               (invalidate cmd))
              ((:sub :add)
               (when (and (x86-argument-matches? arg1 :reg-id :esp)
                          (typep arg2 'x86-argument-constant))
                 (incf cur-cfa (if (eq opcode :sub) imm (- imm))))
               (invalidate cmd))
              ;; Bail out on tail-call
              (:jmp
               (unless (and (<= address addr)
                            (or (and length (< (- addr address) length))
                                (= cur-cfa 4)))
                 (emit-call cmd (get-callee-name addr))
                 (return)))
              ;; Log calls
              (:call
               (emit-call cmd (get-callee-name addr)))
              ;; Save move and lea arithmetic
              ((:mov :lea)
               (let ((val
                      (typecase arg2
                        (x86-argument-register
                         (get-reg (x86-argument-register-id arg2)))
                        (x86-argument-constant
                         (format nil "0x~X" imm))
                        (x86-argument-memory
                         (aif (is-stack-var? arg2)
                              (let ((unwind (when unwinds
                                              (lookup-chunk unwinds (x86-instruction-address cmd)))))
                                (when (if unwind
                                          (= it (unwind-state-cfa unwind))
                                          (= it cur-cfa))
                                  "this"))
                              (let* ((base (x86-argument-memory-base-reg arg2))
                                     (disp (x86-argument-memory-displacement arg2))
                                     (idx (x86-argument-memory-index-reg arg2))
                                     (val
                                      (cond (base
                                             (format nil "~A~{+~A*~A~}~@[~A~]"
                                                     (get-reg base)
                                                     (when idx
                                                       (list (get-reg idx)
                                                             (x86-argument-memory-scale arg2)))
                                                     (when (/= disp 0)
                                                       (format-hex-offset disp :force-sign? t))))
                                            (idx
                                             (format nil "~@[~A+~]~A*~A~@[~A~]"
                                                     (get-reg base)
                                                     (get-reg idx) (x86-argument-memory-scale arg2)
                                                     (when (/= disp 0)
                                                       (format-hex-offset disp :force-sign? t))))
                                            (t
                                             (format-hex-offset disp)))))
                                (if (eq opcode :lea)
                                    val
                                    (concatenate 'string "[" val "]"))))))))
                 (acond ((is-stack-var? arg1)
                         (when (= it 0)
                           (setf stack-top val)))
                        ((typep arg1 'x86-argument-register)
                         (setf (assoc-value reg-state (x86-argument-register-id arg1)) val)))))
              ;;
              (otherwise (invalidate cmd)))))))))
