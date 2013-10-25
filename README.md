CLERIC - Common Lisp Erlang Interface
=====================================

CLERIC is an implementation of the Erlang distribution protocol, comparable with
erl_interface and jinterface.

Homepage: <http://common-lisp.net/project/cleric/>

Please see the `doc/` directory for further documentation. The latest documentation is available [online](http://common-lisp.net/project/cleric/doc/).


How to install
--------------

CLERIC is easily installed with [Quicklisp](http://www.quicklisp.org/).

    > (ql:quickload :cleric)

To check out the latest code, use git:

    $ git clone git://github.com/flambard/CLERIC.git


### Dependencies

- [cl-epmd](https://github.com/flambard/cl-epmd)
- [cl-erlang-term](https://github.com/flambard/cl-erlang-term)
- [usocket](http://common-lisp.net/project/usocket/)
- [binary-data](https://github.com/gigamonkey/monkeylib-binary-data)
- [CL-MD5](http://www.cliki.net/CL-MD5)
- [Alexandria](http://common-lisp.net/project/alexandria/)

Quicklisp will download and install these libraries automatically when installing CLERIC.
