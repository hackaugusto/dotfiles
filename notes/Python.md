
Python managers
===============

My current approach is to just install multiple versions of the interpreter
through custom PKGBUILDs and then use plain old virtual environments.

Pythonz
-------

Pythonz doesn't support the ability to patch the source before compilation.
Which was a problem to install a custom version of python2.7 with tracemalloc
patched.

- patch before compilation [issue #91] and [https://github.com/yyuu/pyenv/tree/master/plugins/python-build#applying-patches-to-python-before-compiling]
- multiple versions [issue #167 and #218] and [https://github.com/s1341/pyenv-alias]

Pyenv
-----

Force the use of ucs4 because arch's is compiled with it

```
PYTHON_CONFIGURE_OPTS="--enable-shared --enable-unicode=ucs4" pyenv install 2.7.11
PYTHON_CONFIGURE_OPTS="--enable-shared --enable-unicode=ucs4" pyenv install 3.5.1
VERSION_ALIAS="2.7.11-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" CC=gcc PYTHON_CFLAGS="-Og -ggdb3" pyenv install 2.7.11
VERSION_ALIAS="3.5.1-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" pyenv install 3.5.1

git clone https://github.com/haypo/pytracemalloc.git
cat pytracemalloc/patches/2.7/pep445.patch | filterdiff --strip=1 | VERSION_ALIAS="2.7.8-trace" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" pyenv install -p -v 2.7.8
```
