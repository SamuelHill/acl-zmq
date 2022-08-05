# acl-zmq: Allegro Common Lisp interface to ZMQ

This library implements an interface to the ZMQ message queueing system for Allegro Common Lisp (ACL) intended for use in the QRG lab at Northwestern University. This code is a fork of [hanshuebner/acl-zmq](https://github.com/hanshuebner/acl-zmq), which itself is a fork of the [cl-zmq](http://repo.or.cz/w/cl-zmq.git) library which is based on [CFFI](http://common-lisp.net/project/cffi/).

acl-zmq has been implemented and tested with Allegro CL 10.1, ZMQ version 141 as of December 2021 on Windows 10.

Like cl-zmq, acl-zmq is distributed under the [LLGPL](http://opensource.franz.com/preamble.html).

The code has not been tested or used a lot, so it is likely to contain bugs. As I am not currently using ACL and ZMQ, I cannot give
much support for it. Please keep this in mind if you use it as a starting point for your project.
