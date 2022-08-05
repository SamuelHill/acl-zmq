;;;; @Author: Samuel Hill
;;;; @Date:   2021-04-28 11:02:34
;;;; @Last Modified by:   Samuel Hill
;;;; @Last Modified time: 2021-05-07 01:36:40

(in-package :cl-user)

(defpackage #:zeromq
  (:nicknames :zmq)
  (:use :common-lisp)
  (:export #:with-socket
           #:open-socket
           #:close-socket
           #:with-context
           #:init-context
           #:term-context
           #:do-polling

           #:msg
           #:msg-size
           #:msg-init-size
           #:msg-data-as-array
           #:msg-data-as-string
           #:msg-close

           #:bind
           #:connect
           #:send
           #:recv
           #:setsockopt

           #:error-again

           #:version))

(in-package :zeromq)

(defparameter *zmq-path* (qrg:make-qrg-path "code-library" "lisp" "zmq"))
(defparameter *zmq-library-path* (qrg:make-qrg-path "code-library" "lisp" "zmq" "libzmq-v141-x64-4_3_2"))

(progn
  (qrg:load-foreign-library (qrg:make-full-file-spec *zmq-library-path* "libsodium" ".dll"))
  (qrg:load-foreign-library (qrg:make-full-file-spec *zmq-library-path* "libzmq-v141-mt-4_3_2" ".dll")))

(defparameter *zmq-files* '("zeromq-ffi"
                            "zeromq-api"))

(defmethod qrg:load-sys ((system (eql :zmq)) &rest keys
                         &key (action :source-if-newer) (verbose t)
                         &allow-other-keys)
  (let ((ff:*pass-structs-by-value* nil))
    (qrg:load-files *zmq-path* *zmq-files*
                    :action action :verbose verbose)
    :zmq))

(defmethod qrg:auxiliary-files ((system (eql :zmq))) nil)

(defmethod qrg:svn-immediate-dependent-systems ((system (eql :zmq))) '())

(defmethod qrg:svn-update-paths-for-system ((system (eql :zmq)) &rest keys) (list *zmq-path*))
