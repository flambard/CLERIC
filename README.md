CLERIC - Common Lisp Erlang Interface
=====================================

CLERIC is an implementation of the Erlang distribution protocol, comparable with
erl_interface and jinterface.

Homepage: <http://common-lisp.net/project/cleric/>

Please see the `doc/` directory for further documentation. The latest documentation is available [online](http://common-lisp.net/project/cleric/doc/).


How to install
--------------

CLERIC is easily installed with [Quicklisp](http://www.quicklisp.org/) or [ASDF-Install](http://common-lisp.net/project/asdf-install/).

Using Quicklisp:

    > (ql:quickload :cleric)

Using ASDF-Install:

    > (asdf-install:install :cleric)

One can also [download](http://common-lisp.net/project/cleric/releases/) a tarball and untar it where ASDF can find it.

To check out the latest code, use git:

    $ git clone git://github.com/flambard/CLERIC.git


### Dependencies

- [cl-erlang-term](https://github.com/flambard/cl-erlang-term)
- [usocket](http://common-lisp.net/project/usocket/)
- [FLEXI-STREAMS](http://weitz.de/flexi-streams/)
- [CL-MD5](http://www.cliki.net/CL-MD5)
- [Alexandria](http://common-lisp.net/project/alexandria/)

Quicklisp and ASDF-Install will download and install these libraries automatically when installing CLERIC.
