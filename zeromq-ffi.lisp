;;;; @Author: Samuel Hill
;;;; @Date:   2021-03-12 12:45:13
;;;; @Last Modified by:   Samuel Hill
;;;; @Last Modified time: 2021-04-30 12:25:00

(in-package :zeromq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                Allegro specific setup for accessing foreign functions                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-type (type &rest args)
  (let ((type (if (and (listp type)
                       (eq 'quote (first type)))
                  (second type)
                  type)))
    (cond
      ((eq type :pointer)
       `(* ,(or (first args) :void)))
      ((listp type)
       (assert (eql :pointer (first type)) () "expected :pointer, got ~A" (first type))
       `(* ,(map-type (second type))))
      (t
       (case type
         #+64bit
         (:int64
          :long)
         (:uchar
          :unsigned-char)
         (t
          type))))))

(defun map-argument (argument)
  (destructuring-bind (name type &rest args) argument
    (list name
          (cond
            ((eq :count (first args))
             `(:array ,(map-type type) ,(second args)))
            ((eq :pointer (first args))
             `(* ,(or (second args) :void)))
            (t
             (apply #'map-type type args))))))

(defmacro defcfun ((c-name lisp-name) return-type &rest arguments)
  `(ff:def-foreign-call (,lisp-name ,c-name)
       ,(mapcar #'map-argument arguments)
     :returning (,(map-type return-type))
     :strings-convert nil))

(defmacro defcfun* ((c-name lisp-name) return-type &rest arguments)
  (let ((lisp-stub-name (intern (format nil "%~A" lisp-name) :zeromq))
        (argument-names (mapcar #'first arguments))
        (return-value (gensym)))
    `(progn
       (ff:def-foreign-call (,lisp-stub-name ,c-name)
           ,(mapcar #'map-argument arguments)
         :returning (,(map-type return-type))
         :strings-convert nil
         :error-value :errno)
       (defun ,lisp-name ,argument-names
         (multiple-value-bind (,return-value errno) (,lisp-stub-name ,@argument-names)
           (if ,(if (eq return-type :pointer)
                    `(zerop ,return-value)
                    `(minusp ,return-value))
               (if (eql errno excl::*eagain*)
                   (error 'error-again)
                   (error 'zmq-syscall-error :call-name ,c-name :errno errno))
               ,return-value))))))

(defmacro defcstruct (name &rest args)
  `(ff:def-foreign-type ,name (:struct ,@(mapcar #'map-argument args))))

(defmacro defcallback (name return-type args &body body)
  (assert (eq :void return-type))
  `(ff:defun-foreign-callable ,name ,(mapcar #'map-argument args)
     ,@body))

(defmacro with-foreign-string ((c-string lisp-string) &body body)
  `(let ((,c-string (excl:string-to-native ,lisp-string)))
     (unwind-protect
          (progn ,@body)
       (excl:aclfree ,c-string))))

(defmacro with-foreign-slots ((slots address type) &body body)
  (flet
      ((gen-slot-accessor (slot-name)
         `(,slot-name (ff:fslot-value-typed ',type :c ,address ',slot-name))))
    `(symbol-macrolet ,(mapcar #'gen-slot-accessor slots)
       ,@body)))

(defvar *named-constants* nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun constant-name-to-keyword (name)
    (let ((constant-name (symbol-name name)))
      (assert (eql #\+ (aref constant-name 0)))
      (assert (eql #\+ (aref constant-name (1- (length constant-name)))))
      (intern (subseq constant-name 1 (1- (length constant-name))) :keyword))))

(defmacro defconstant* (name value &optional documentation)
  `(progn
     (defconstant ,name ,value ,@(list documentation))
     (pushnew (cons ,(constant-name-to-keyword name) ,name) *named-constants* :test #'equal)))

(defun lookup-constant (name)
  (assert (keywordp name))
  (or (cdr (assoc name *named-constants*))
      (error "invalid named constant ~A" name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                      0MQ errors                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +hausnumero+ 156384712)

;; + Native 0MQ error+ codes.
(defconstant* +emthread+ (+ +hausnumero+ 50))
(defconstant* +efsm+ (+ +hausnumero+ 51))
(defconstant* +enocompatproto+ (+ +hausnumero+ 52))

(defcfun ("zmq_strerror" %strerror) :pointer
  (errnum :int))

(define-condition error-again (error)
  ())

(define-condition zmq-syscall-error (excl:syscall-error)
  ((call-name :initarg :call-name :reader error-call-name))
  (:report (lambda (c stream)
             (format stream "ZMQ call ~S failed: ~A"
                     (error-call-name c)
                     (excl:native-to-string (%strerror (excl:syscall-error-errno c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                0MQ message definition                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant* +max-vsm-size+ 30)

;;  Message types. These integers may be stored in 'content' member of the
;;  message instead of regular pointer to the data.
(defconstant* +delimiter+ 31)
(defconstant* +vsm+ 32)

;; Message flags. ZMQ_MSG_SHARED is strictly speaking not a message flag
;; (it has no equivalent in the wire format), however, making  it a flag
;; allows us to pack the stucture tigher and thus improve performance.
(defconstant* +msg-more+ 1)
(defconstant* +msg-shared+ 128)

(defcstruct %msg
  (content  :pointer)
  (shared   :uchar)
  (vsm-size :uchar)
  (vsm-data :uchar :count 30))  ;; FIXME max-vsm-size

(defcfun ("zmq_msg_init" %msg-init) :int
  (msg  %msg))

(defcfun* ("zmq_msg_init_size" %msg-init-size) :int
  (msg  %msg)
  (size :long))

(defcallback zmq-free :void ((ptr :pointer) (hint :pointer))
  (declare (ignorable hint))
  (excl:aclfree ptr))

(defcfun ("zmq_msg_init_data" %msg-init-data) :int
  (msg  %msg)
  (data :pointer)
  (size :long)
  (ffn  :pointer)           ; zmq_free_fn
  (hint :pointer))

(defcfun* ("zmq_msg_close" %msg-close) :int
  (msg  %msg))

(defcfun ("zmq_msg_move" %msg-move) :int
  (dest %msg)
  (src  %msg))

(defcfun ("zmq_msg_copy" %msg-copy) :int
  (dest %msg)
  (src  %msg))

(defcfun ("zmq_msg_data" %msg-data) :pointer
  (msg  %msg))

(defcfun ("zmq_msg_size" %msg-size) :int
  (msg  %msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                   0MQ infrastructure - initialisation & termination                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun* ("zmq_init" %init) :pointer
  (io-threads   :int))

(defcfun ("zmq_term" %term) :int
  (context  :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 0MQ socket definition                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant* +p2p+ 0)
(defconstant* +pub+ 1)
(defconstant* +sub+ 2)
(defconstant* +req+ 3)
(defconstant* +rep+ 4)
(defconstant* +xreq+ 5)
(defconstant* +xrep+ 6)
(defconstant* +upstream+ 7)
(defconstant* +downstream+ 8)
(defconstant* +push+ 7)
(defconstant* +pull+ 8)

(defconstant* +hwm+ 1)
(defconstant* +swap+ 3)
(defconstant* +affinity+ 4)
(defconstant* +identity+ 5)
(defconstant* +subscribe+ 6)
(defconstant* +unsubscribe+ 7)
(defconstant* +rate+ 8)
(defconstant* +recovery-ivl+ 9)
(defconstant* +mcast-loop+ 10)
(defconstant* +sndbuf+ 11)
(defconstant* +rcvbuf+ 12)
(defconstant* +rcvmore+ 13)

(defconstant* +noblock+ 1)
(defconstant* +sndmore+ 2)

(defcfun* ("zmq_socket" %zmq_socket) :pointer
  (context  :pointer)
  (type     :int))

(defcfun ("zmq_close" %zmq_close) :int
  (s    :pointer))

(defcfun* ("zmq_setsockopt" %setsockopt) :int
  (s        :pointer)
  (option   :int)
  (optval   :pointer)
  (optvallen    :long))

(defcfun* ("zmq_getsockopt" %getsockopt) :int
  (s        :pointer)
  (option   :int)
  (optval   :pointer)
  (optvallen    :pointer))

(defcfun* ("zmq_bind" %bind) :int
  (s    :pointer)
  (addr :pointer :char))

(defcfun* ("zmq_connect" %connect) :int
  (s    :pointer)
  (addr :pointer :char))


(defcfun* ("zmq_send" %send) :int
  (s        :pointer)
  (buf      :pointer :char)
  (msg      :int)
  (flags    :int))

(defcfun* ("zmq_recv" %recv) :int
  (s        :pointer)
  (msg      %msg)
  (flags    :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   I/O multiplexing                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant* +pollin+ 1)
(defconstant* +pollout+ 2)
(defconstant* +pollerr+ 4)

(defcstruct %pollitem
  (socket   :pointer)
  (fd       :int)
  (events   :short)
  (revents  :short))

(defcfun* ("zmq_poll" %poll) :int
  (items    :pointer)
  (nitems   :int)
  (timeout  :long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    Helper functions                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_version" %version) :void
  (major    :pointer :int)
  (minor    :pointer :int)
  (patch    :pointer :int))

(defcfun* ("zmq_device" %device) :int
  (device   :int)
  (insocket :pointer)
  (outsocket    :pointer))

