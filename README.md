CLERIC - Common Lisp Erlang Interface
=====================================

CLERIC is an implementation of the Erlang distribution protocol, comparable with
erl_interface and jinterface.

CLERIC also includes a [BERT](http://bert-rpc.org/) serializer.

Homepage: [http://common-lisp.net/project/cleric/](http://common-lisp.net/project/cleric/)


How to install
--------------

The easiest way to install CLERIC is to use [ASDF-Install](http://common-lisp.net/project/asdf-install/):

    > (asdf-install:install :cleric)

One can also [download](http://common-lisp.net/project/cleric/releases/) a tarball and untar it where ASDF can find it.

To check out the latest code, use git:

    $ git clone git://github.com/flambard/CLERIC.git


### Dependencies

- [usocket](http://common-lisp.net/project/usocket/)
- [CL-MD5](http://www.cliki.net/CL-MD5)
- [IEEE-Floats](http://common-lisp.net/project/ieee-floats/)

ASDF-Install will download and install these libraries automatically when installing CLERIC.
