#!/usr/bin/python
'''
From gdb 7 onwards, gdb's build can be configured --with-python, allowing gdb
to be extended with Python code e.g. for library-specific data visualizations,
such as for the C++ STL types.  Documentation on this API can be seen at:
http://sourceware.org/gdb/current/onlinedocs/gdb/Python-API.html


This python module deals with the case when the process being debugged (the
"inferior process" in gdb parlance) is itself python, or more specifically,
linked against libpython.  In this situation, almost every item of data is a
(PyObject*), and having the debugger merely print their addresses is not very
enlightening.

This module embeds knowledge about the implementation details of libpython so
that we can emit useful visualizations e.g. a string, a list, a dict, a frame
giving file/line information and the state of local variables

In particular, given a gdb.Value corresponding to a PyObject* in the inferior
process, we can generate a "proxy value" within the gdb process.  For example,
given a PyObject* in the inferior process that is in fact a PyListObject*
holding three PyObject* that turn out to be PyBytesObject* instances, we can
generate a proxy value within the gdb process that is a list of bytes
instances:
  [b"foo", b"bar", b"baz"]

Doing so can be expensive for complicated graphs of objects, and could take
some time, so we also have a "write_repr" method that writes a representation
of the data to a file-like object.  This allows us to stop the traversal by
having the file-like object raise an exception if it gets too much data.

With both "proxyval" and "write_repr" we keep track of the set of all addresses
visited so far in the traversal, to avoid infinite recursion due to cycles in
the graph of object references.

We try to defer gdb.lookup_type() invocations for python types until as late as
possible: for a dynamically linked python binary, when the process starts in
the debugger, the libpython.so hasn't been dynamically loaded yet, so none of
the type names are known to the debugger

The module also extends gdb with some python-specific commands.
'''

# NOTE: some gdbs are linked with Python 3, so this file should be dual-syntax
# compatible (2.6+ and 3.0+).  See #19308.

from __future__ import print_function
import gdb
import os
import locale
import sys

if sys.version_info[0] >= 3:
    unichr = chr
    xrange = range
    long = int

# Look up the gdb.Type for some standard types:
# Those need to be refreshed as types (pointer sizes) may change when
# gdb loads different executables

def _type_char_ptr():
    return gdb.lookup_type('char').pointer()  # char*


def _type_unsigned_char_ptr():
    return gdb.lookup_type('unsigned char').pointer()  # unsigned char*


def _type_unsigned_short_ptr():
    return gdb.lookup_type('unsigned short').pointer()


def _type_unsigned_int_ptr():
    return gdb.lookup_type('unsigned int').pointer()


def _sizeof_void_p():
    return gdb.lookup_type('void').pointer().sizeof


# value computed later, see PyUnicodeObjectPtr.proxy()
_is_pep393 = None

Py_TPFLAGS_HEAPTYPE = (1 << 9)
Py_TPFLAGS_LONG_SUBCLASS     = (1 << 24)
Py_TPFLAGS_LIST_SUBCLASS     = (1 << 25)
Py_TPFLAGS_TUPLE_SUBCLASS    = (1 << 26)
Py_TPFLAGS_BYTES_SUBCLASS    = (1 << 27)
Py_TPFLAGS_UNICODE_SUBCLASS  = (1 << 28)
Py_TPFLAGS_DICT_SUBCLASS     = (1 << 29)
Py_TPFLAGS_BASE_EXC_SUBCLASS = (1 << 30)
Py_TPFLAGS_TYPE_SUBCLASS     = (1 << 31)


MAX_OUTPUT_LEN=1024

hexdigits = "0123456789abcdef"

ENCODING = locale.getpreferredencoding()

FRAME_INFO_OPTIMIZED_OUT = '(frame information optimized out)'
UNABLE_READ_INFO_PYTHON_FRAME = 'Unable to read information on python frame'
EVALFRAME = '_PyEval_EvalFrameDefault'

class NullPyObjectPtr(RuntimeError):
    pass


def safety_limit(val):
    # Given an integer value from the process being debugged, limit it to some
    # safety threshold so that arbitrary breakage within said process doesn't
    # break the gdb process too much (e.g. sizes of iterations, sizes of lists)
    return min(val, 1000)


def safe_range(val):
    # As per range, but don't trust the value too much: cap it to a safety
    # threshold in case the data was corrupted
    return xrange(safety_limit(int(val)))

if sys.version_info[0] >= 3:
    def write_unicode(file, text):
        file.write(text)
else:
    def write_unicode(file, text):
        # Write a byte or unicode string to file. Unicode strings are encoded to
        # ENCODING encoding with 'backslashreplace' error handler to avoid
        # UnicodeEncodeError.
        if isinstance(text, unicode):
            text = text.encode(ENCODING, 'backslashreplace')
        file.write(text)

try:
    os_fsencode = os.fsencode
except AttributeError:
    def os_fsencode(filename):
        if not isinstance(filename, unicode):
            return filename
        encoding = sys.getfilesystemencoding()
        if encoding == 'mbcs':
            # mbcs doesn't support surrogateescape
            return filename.encode(encoding)
        encoded = []
        for char in filename:
            # surrogateescape error handler
            if 0xDC80 <= ord(char) <= 0xDCFF:
                byte = chr(ord(char) - 0xDC00)
            else:
                byte = char.encode(encoding)
            encoded.append(byte)
        return ''.join(encoded)

class StringTruncated(RuntimeError):
    pass

class TruncatedStringIO(object):
    '''Similar to io.StringIO, but can truncate the output by raising a
    StringTruncated exception'''
    def __init__(self, maxlen=None):
        self._val = ''
        self.maxlen = maxlen

    def write(self, data):
        if self.maxlen:
            if len(data) + len(self._val) > self.maxlen:
                # Truncation:
                self._val += data[0:self.maxlen - len(self._val)]
                raise StringTruncated()

        self._val += data

    def getvalue(self):
        return self._val

class PyObjectPtr(object):
    """
    Class wrapping a gdb.Value that's either a (PyObject*) within the
    inferior process, or some subclass pointer e.g. (PyBytesObject*)

    There will be a subclass for every refined PyObject type that we care
    about.

    Note that at every stage the underlying pointer could be NULL, point
    to corrupt data, etc; this is the debugger, after all.
    """
    _typename = 'PyObject'

    def __init__(self, gdbval, cast_to=None):
        if cast_to:
            self._gdbval = gdbval.cast(cast_to)
        else:
            self._gdbval = gdbval

    def field(self, name):
        '''
        Get the gdb.Value for the given field within the PyObject, coping with
        some python 2 versus python 3 differences.

        Various libpython types are defined using the "PyObject_HEAD" and
        "PyObject_VAR_HEAD" macros.

        In Python 2, this these are defined so that "ob_type" and (for a var
        object) "ob_size" are fields of the type in question.

        In Python 3, this is defined as an embedded PyVarObject type thus:
           PyVarObject ob_base;
        so that the "ob_size" field is located insize the "ob_base" field, and
        the "ob_type" is most easily accessed by casting back to a (PyObject*).
        '''
        if self.is_null():
            raise NullPyObjectPtr(self)

        if name == 'ob_type':
            pyo_ptr = self._gdbval.cast(PyObjectPtr.get_gdb_type())
            return pyo_ptr.dereference()[name]

        if name == 'ob_size':
            pyo_ptr = self._gdbval.cast(PyVarObjectPtr.get_gdb_type())
            return pyo_ptr.dereference()[name]

        # General case: look it up inside the object:
        return self._gdbval.dereference()[name]

    def pyop_field(self, name):
        '''
        Get a PyObjectPtr for the given PyObject* field within this PyObject,
        coping with some python 2 versus python 3 differences.
        '''
        return PyObjectPtr.from_pyobject_ptr(self.field(name))

    def write_field_repr(self, name, out, visited):
        '''
        Extract the PyObject* field named "name", and write its representation
        to file-like object "out"
        '''
        field_obj = self.pyop_field(name)
        field_obj.write_repr(out, visited)

    def get_truncated_repr(self, maxlen):
        '''
        Get a repr-like string for the data, but truncate it at "maxlen" bytes
        (ending the object graph traversal as soon as you do)
        '''
        out = TruncatedStringIO(maxlen)
        try:
            self.write_repr(out, set())
        except StringTruncated:
            # Truncation occurred:
            return out.getvalue() + '...(truncated)'

        # No truncation occurred:
        return out.getvalue()

    def type(self):
        return PyTypeObjectPtr(self.field('ob_type'))

    def is_null(self):
        return 0 == long(self._gdbval)

    def is_optimized_out(self):
        '''
        Is the value of the underlying PyObject* visible to the debugger?

        This can vary with the precise version of the compiler used to build
        Python, and the precise version of gdb.

        See e.g. https://bugzilla.redhat.com/show_bug.cgi?id=556975 with
        PyEval_EvalFrameEx's "f"
        '''
        return self._gdbval.is_optimized_out

    def safe_tp_name(self):
        try:
            ob_type = self.type()
            tp_name = ob_type.field('tp_name')
            return tp_name.string()
        # NullPyObjectPtr: NULL tp_name?
        # RuntimeError: Can't even read the object at all?
        # UnicodeDecodeError: Failed to decode tp_name bytestring
        except (NullPyObjectPtr, RuntimeError, UnicodeDecodeError):
            return 'unknown'

    def proxyval(self, visited):
        '''
        Scrape a value from the inferior process, and try to represent it
        within the gdb process, whilst (hopefully) avoiding crashes when
        the remote data is corrupt.

        Derived classes will override this.

        For example, a PyIntObject* with ob_ival 42 in the inferior process
        should result in an int(42) in this process.

        visited: a set of all gdb.Value pyobject pointers already visited
        whilst generating this value (to guard against infinite recursion when
        visiting object graphs with loops).  Analogous to Py_ReprEnter and
        Py_ReprLeave
        '''

        class FakeRepr(object):
            """
            Class representing a non-descript PyObject* value in the inferior
            process for when we don't have a custom scraper, intended to have
            a sane repr().
            """

            def __init__(self, tp_name, address):
                self.tp_name = tp_name
                self.address = address

            def __repr__(self):
                # For the NULL pointer, we have no way of knowing a type, so
                # special-case it as per
                # http://bugs.python.org/issue8032#msg100882
                if self.address == 0:
                    return '0x0'
                return '<%s at remote 0x%x>' % (self.tp_name, self.address)

        return FakeRepr(self.safe_tp_name(),
                        long(self._gdbval))

    def write_repr(self, out, visited):
        '''
        Write a string representation of the value scraped from the inferior
        process to "out", a file-like object.
        '''
        # Default implementation: generate a proxy value and write its repr
        # However, this could involve a lot of work for complicated objects,
        # so for derived classes we specialize this
        return out.write(repr(self.proxyval(visited)))

    @classmethod
    def subclass_from_type(cls, t):
        '''
        Given a PyTypeObjectPtr instance wrapping a gdb.Value that's a
        (PyTypeObject*), determine the corresponding subclass of PyObjectPtr
        to use

        Ideally, we would look up the symbols for the global types, but that
        isn't working yet:
          (gdb) python print gdb.lookup_symbol('PyList_Type')[0].value
          Traceback (most recent call last):
            File "<string>", line 1, in <module>
          NotImplementedError: Symbol type not yet supported in Python scripts.
          Error while executing Python code.

        For now, we use tp_flags, after doing some string comparisons on the
        tp_name for some special-cases that don't seem to be visible through
        flags
        '''
        try:
            tp_name = t.field('tp_name').string()
            tp_flags = int(t.field('tp_flags'))
        # RuntimeError: NULL pointers
        # UnicodeDecodeError: string() fails to decode the bytestring
        except (RuntimeError, UnicodeDecodeError):
            # Handle any kind of error e.g. NULL ptrs by simply using the base
            # class
            return cls

        #print('tp_flags = 0x%08x' % tp_flags)
        #print('tp_name = %r' % tp_name)

        name_map = {'bool': PyBoolObjectPtr,
                    'classobj': PyClassObjectPtr,
                    'NoneType': PyNoneStructPtr,
                    'frame': PyFrameObjectPtr,
                    'set' : PySetObjectPtr,
                    'frozenset' : PySetObjectPtr,
                    'builtin_function_or_method' : PyCFunctionObjectPtr,
                    'method-wrapper': wrapperobject,
                    }
        if tp_name in name_map:
            return name_map[tp_name]

        if tp_flags & Py_TPFLAGS_HEAPTYPE:
            return HeapTypeObjectPtr

        if tp_flags & Py_TPFLAGS_LONG_SUBCLASS:
            return PyLongObjectPtr
        if tp_flags & Py_TPFLAGS_LIST_SUBCLASS:
            return PyListObjectPtr
        if tp_flags & Py_TPFLAGS_TUPLE_SUBCLASS:
            return PyTupleObjectPtr
        if tp_flags & Py_TPFLAGS_BYTES_SUBCLASS:
            return PyBytesObjectPtr
        if tp_flags & Py_TPFLAGS_UNICODE_SUBCLASS:
            return PyUnicodeObjectPtr
        if tp_flags & Py_TPFLAGS_DICT_SUBCLASS:
            return PyDictObjectPtr
        if tp_flags & Py_TPFLAGS_BASE_EXC_SUBCLASS:
            return PyBaseExceptionObjectPtr
        #if tp_flags & Py_TPFLAGS_TYPE_SUBCLASS:
        #    return PyTypeObjectPtr

        # Use the base class:
        return cls

    @classmethod
    def from_pyobject_ptr(cls, gdbval):
        '''
        Try to locate the appropriate derived class dynamically, and cast
        the pointer accordingly.
        '''
        try:
            p = PyObjectPtr(gdbval)
            cls = cls.subclass_from_type(p.type())
            return cls(gdbval, cast_to=cls.get_gdb_type())
        except RuntimeError:
            # Handle any kind of error e.g. NULL ptrs by simply using the base
            # class
            pass
        return cls(gdbval)

    @classmethod
    def get_gdb_type(cls):
        return gdb.lookup_type(cls._typename).pointer()

    def as_address(self):
        return long(self._gdbval)

class PyVarObjectPtr(PyObjectPtr):
    _typename = 'PyVarObject'

class ProxyAlreadyVisited(object):
    '''
    Placeholder proxy to use when protecting against infinite recursion due to
    loops in the object graph.

    Analogous to the values emitted by the users of Py_ReprEnter and Py_ReprLeave
    '''
    def __init__(self, rep):
        self._rep = rep

    def __repr__(self):
        return self._rep


def _write_instance_repr(out, visited, name, pyop_attrdict, address):
    '''Shared code for use by all classes:
    write a representation to file-like object "out"'''
    out.write('<')
    out.write(name)

    # Write dictionary of instance attributes:
    if isinstance(pyop_attrdict, PyDictObjectPtr):
        out.write('(')
        first = True
        for pyop_arg, pyop_val in pyop_attrdict.iteritems():
            if not first:
                out.write(', ')
            first = False
            out.write(pyop_arg.proxyval(visited))
            out.write('=')
            pyop_val.write_repr(out, visited)
        out.write(')')
    out.write(' at remote 0x%x>' % address)


class InstanceProxy(object):

    def __init__(self, cl_name, attrdict, address):
        self.cl_name = cl_name
        self.attrdict = attrdict
        self.address = address

    def __repr__(self):
        if isinstance(self.attrdict, dict):
            kwargs = ', '.join(["%s=%r" % (arg, val)
                                for arg, val in self.attrdict.items()])
            return '<%s(%s) at remote 0x%x>' % (self.cl_name,
                                                kwargs, self.address)
        else:
            return '<%s at remote 0x%x>' % (self.cl_name,
                                            self.address)

def _PyObject_VAR_SIZE(typeobj, nitems):
    if _PyObject_VAR_SIZE._type_size_t is None:
        _PyObject_VAR_SIZE._type_size_t = gdb.lookup_type('size_t')

    return ( ( typeobj.field('tp_basicsize') +
               nitems * typeobj.field('tp_itemsize') +
               (_sizeof_void_p() - 1)
             ) & ~(_sizeof_void_p() - 1)
           ).cast(_PyObject_VAR_SIZE._type_size_t)
_PyObject_VAR_SIZE._type_size_t = None

class HeapTypeObjectPtr(PyObjectPtr):
    _typename = 'PyObject'

    def get_attr_dict(self):
        '''
        Get the PyDictObject ptr representing the attribute dictionary
        (or None if there's a problem)
        '''
        try:
            typeobj = self.type()
            dictoffset = int_from_int(typeobj.field('tp_dictoffset'))
            if dictoffset != 0:
                if dictoffset < 0:
                    type_PyVarObject_ptr = gdb.lookup_type('PyVarObject').pointer()
                    tsize = int_from_int(self._gdbval.cast(type_PyVarObject_ptr)['ob_size'])
                    if tsize < 0:
                        tsize = -tsize
                    size = _PyObject_VAR_SIZE(typeobj, tsize)
                    dictoffset += size
                    assert dictoffset > 0
                    assert dictoffset % _sizeof_void_p() == 0

                dictptr = self._gdbval.cast(_type_char_ptr()) + dictoffset
                PyObjectPtrPtr = PyObjectPtr.get_gdb_type().pointer()
                dictptr = dictptr.cast(PyObjectPtrPtr)
                return PyObjectPtr.from_pyobject_ptr(dictptr.dereference())
        except RuntimeError:
            # Corrupt data somewhere; fail safe
            pass

        # Not found, or some kind of error:
        return None

    def proxyval(self, visited):
        '''
        Support for classes.

        Currently we just locate the dictionary using a transliteration to
        python of _PyObject_GetDictPtr, ignoring descriptors
        '''
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('<...>')
        visited.add(self.as_address())

        pyop_attr_dict = self.get_attr_dict()
        if pyop_attr_dict:
            attr_dict = pyop_attr_dict.proxyval(visited)
        else:
            attr_dict = {}
        tp_name = self.safe_tp_name()

        # Class:
        return InstanceProxy(tp_name, attr_dict, long(self._gdbval))

    def write_repr(self, out, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('<...>')
            return
        visited.add(self.as_address())

        pyop_attrdict = self.get_attr_dict()
        _write_instance_repr(out, visited,
                             self.safe_tp_name(), pyop_attrdict, self.as_address())

class ProxyException(Exception):
    def __init__(self, tp_name, args):
        self.tp_name = tp_name
        self.args = args

    def __repr__(self):
        return '%s%r' % (self.tp_name, self.args)

class PyBaseExceptionObjectPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyBaseExceptionObject* i.e. an exception
    within the process being debugged.
    """
    _typename = 'PyBaseExceptionObject'

    def proxyval(self, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('(...)')
        visited.add(self.as_address())
        arg_proxy = self.pyop_field('args').proxyval(visited)
        return ProxyException(self.safe_tp_name(),
                              arg_proxy)

    def write_repr(self, out, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('(...)')
            return
        visited.add(self.as_address())

        out.write(self.safe_tp_name())
        self.write_field_repr('args', out, visited)

class PyClassObjectPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyClassObject* i.e. a <classobj>
    instance within the process being debugged.
    """
    _typename = 'PyClassObject'


class BuiltInFunctionProxy(object):
    def __init__(self, ml_name):
        self.ml_name = ml_name

    def __repr__(self):
        return "<built-in function %s>" % self.ml_name

class BuiltInMethodProxy(object):
    def __init__(self, ml_name, pyop_m_self):
        self.ml_name = ml_name
        self.pyop_m_self = pyop_m_self

    def __repr__(self):
        return ('<built-in method %s of %s object at remote 0x%x>'
                % (self.ml_name,
                   self.pyop_m_self.safe_tp_name(),
                   self.pyop_m_self.as_address())
                )

class PyCFunctionObjectPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyCFunctionObject*
    (see Include/methodobject.h and Objects/methodobject.c)
    """
    _typename = 'PyCFunctionObject'

    def proxyval(self, visited):
        m_ml = self.field('m_ml') # m_ml is a (PyMethodDef*)
        try:
            ml_name = m_ml['ml_name'].string()
        except UnicodeDecodeError:
            ml_name = '<ml_name:UnicodeDecodeError>'

        pyop_m_self = self.pyop_field('m_self')
        if pyop_m_self.is_null():
            return BuiltInFunctionProxy(ml_name)
        else:
            return BuiltInMethodProxy(ml_name, pyop_m_self)


class PyCodeObjectPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyCodeObject* i.e. a <code> instance
    within the process being debugged.
    """
    _typename = 'PyCodeObject'

    def addr2line(self, addrq):
        '''
        Get the line number for a given bytecode offset

        Analogous to PyCode_Addr2Line; translated from pseudocode in
        Objects/lnotab_notes.txt
        '''
        co_linetable = self.pyop_field('co_linetable').proxyval(set())

        # Initialize lineno to co_firstlineno as per PyCode_Addr2Line
        # not 0, as lnotab_notes.txt has it:
        lineno = int_from_int(self.field('co_firstlineno'))

        if addrq < 0:
            return lineno
        addr = 0
        for addr_incr, line_incr in zip(co_linetable[::2], co_linetable[1::2]):
            if addr_incr == 255:
                break
            addr += ord(addr_incr)
            line_delta = ord(line_incr)
            if line_delta == 128:
                line_delta = 0
            elif line_delta > 128:
                line_delta -= 256
            lineno += line_delta
            if addr > addrq:
                return lineno
        assert False, "Unreachable"


class PyDictObjectPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyDictObject* i.e. a dict instance
    within the process being debugged.
    """
    _typename = 'PyDictObject'

    def iteritems(self):
        '''
        Yields a sequence of (PyObjectPtr key, PyObjectPtr value) pairs,
        analogous to dict.iteritems()
        '''
        keys = self.field('ma_keys')
        values = self.field('ma_values')
        entries, nentries = self._get_entries(keys)
        for i in safe_range(nentries):
            ep = entries[i]
            if long(values):
                pyop_value = PyObjectPtr.from_pyobject_ptr(values[i])
            else:
                pyop_value = PyObjectPtr.from_pyobject_ptr(ep['me_value'])
            if not pyop_value.is_null():
                pyop_key = PyObjectPtr.from_pyobject_ptr(ep['me_key'])
                yield (pyop_key, pyop_value)

    def proxyval(self, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('{...}')
        visited.add(self.as_address())

        result = {}
        for pyop_key, pyop_value in self.iteritems():
            proxy_key = pyop_key.proxyval(visited)
            proxy_value = pyop_value.proxyval(visited)
            result[proxy_key] = proxy_value
        return result

    def write_repr(self, out, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('{...}')
            return
        visited.add(self.as_address())

        out.write('{')
        first = True
        for pyop_key, pyop_value in self.iteritems():
            if not first:
                out.write(', ')
            first = False
            pyop_key.write_repr(out, visited)
            out.write(': ')
            pyop_value.write_repr(out, visited)
        out.write('}')

    def _get_entries(self, keys):
        dk_nentries = int(keys['dk_nentries'])
        dk_size = int(keys['dk_size'])
        try:
            # <= Python 3.5
            return keys['dk_entries'], dk_size
        except RuntimeError:
            # >= Python 3.6
            pass

        if dk_size <= 0xFF:
            offset = dk_size
        elif dk_size <= 0xFFFF:
            offset = 2 * dk_size
        elif dk_size <= 0xFFFFFFFF:
            offset = 4 * dk_size
        else:
            offset = 8 * dk_size

        ent_addr = keys['dk_indices'].address
        ent_addr = ent_addr.cast(_type_unsigned_char_ptr()) + offset
        ent_ptr_t = gdb.lookup_type('PyDictKeyEntry').pointer()
        ent_addr = ent_addr.cast(ent_ptr_t)

        return ent_addr, dk_nentries


class PyListObjectPtr(PyObjectPtr):
    _typename = 'PyListObject'

    def __getitem__(self, i):
        # Get the gdb.Value for the (PyObject*) with the given index:
        field_ob_item = self.field('ob_item')
        return field_ob_item[i]

    def proxyval(self, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('[...]')
        visited.add(self.as_address())

        result = [PyObjectPtr.from_pyobject_ptr(self[i]).proxyval(visited)
                  for i in safe_range(int_from_int(self.field('ob_size')))]
        return result

    def write_repr(self, out, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('[...]')
            return
        visited.add(self.as_address())

        out.write('[')
        for i in safe_range(int_from_int(self.field('ob_size'))):
            if i > 0:
                out.write(', ')
            element = PyObjectPtr.from_pyobject_ptr(self[i])
            element.write_repr(out, visited)
        out.write(']')

class PyLongObjectPtr(PyObjectPtr):
    _typename = 'PyLongObject'

    def proxyval(self, visited):
        '''
        Python's Include/longobjrep.h has this declaration:
           struct _longobject {
               PyObject_VAR_HEAD
               digit ob_digit[1];
           };

        with this description:
            The absolute value of a number is equal to
                 SUM(for i=0 through abs(ob_size)-1) ob_digit[i] * 2**(SHIFT*i)
            Negative numbers are represented with ob_size < 0;
            zero is represented by ob_size == 0.

        where SHIFT can be either:
            #define PyLong_SHIFT        30
            #define PyLong_SHIFT        15
        '''
        ob_size = long(self.field('ob_size'))
        if ob_size == 0:
            return 0

        ob_digit = self.field('ob_digit')

        if gdb.lookup_type('digit').sizeof == 2:
            SHIFT = 15
        else:
            SHIFT = 30

        digits = [long(ob_digit[i]) * 2**(SHIFT*i)
                  for i in safe_range(abs(ob_size))]
        result = sum(digits)
        if ob_size < 0:
            result = -result
        return result

    def write_repr(self, out, visited):
        # Write this out as a Python 3 int literal, i.e. without the "L" suffix
        proxy = self.proxyval(visited)
        out.write("%s" % proxy)


class PyBoolObjectPtr(PyLongObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyBoolObject* i.e. one of the two
    <bool> instances (Py_True/Py_False) within the process being debugged.
    """
    def proxyval(self, visited):
        if PyLongObjectPtr.proxyval(self, visited):
            return True
        else:
            return False

class PyNoneStructPtr(PyObjectPtr):
    """
    Class wrapping a gdb.Value that's a PyObject* pointing to the
    singleton (we hope) _Py_NoneStruct with ob_type PyNone_Type
    """
    _typename = 'PyObject'

    def proxyval(self, visited):
        return None


class PyFrameObjectPtr(PyObjectPtr):
    _typename = 'PyFrameObject'

    def __init__(self, gdbval, cast_to=None):
        PyObjectPtr.__init__(self, gdbval, cast_to)

        if not self.is_optimized_out():
            self.co = PyCodeObjectPtr.from_pyobject_ptr(self.field('f_code'))
            self.co_name = self.co.pyop_field('co_name')
            self.co_filename = self.co.pyop_field('co_filename')

            self.f_lineno = int_from_int(self.field('f_lineno'))
            self.f_lasti = int_from_int(self.field('f_lasti'))
            self.co_nlocals = int_from_int(self.co.field('co_nlocals'))
            self.co_varnames = PyTupleObjectPtr.from_pyobject_ptr(self.co.field('co_varnames'))

    def iter_locals(self):
        '''
        Yield a sequence of (name,value) pairs of PyObjectPtr instances, for
        the local variables of this frame
        '''
        if self.is_optimized_out():
            return

        f_localsplus = self.field('f_localsplus')
        for i in safe_range(self.co_nlocals):
            pyop_value = PyObjectPtr.from_pyobject_ptr(f_localsplus[i])
            if not pyop_value.is_null():
                pyop_name = PyObjectPtr.from_pyobject_ptr(self.co_varnames[i])
                yield (pyop_name, pyop_value)

    def iter_globals(self):
        '''
        Yield a sequence of (name,value) pairs of PyObjectPtr instances, for
        the global variables of this frame
        '''
        if self.is_optimized_out():
            return ()

        pyop_globals = self.pyop_field('f_globals')
        return pyop_globals.iteritems()

    def iter_builtins(self):
        '''
        Yield a sequence of (name,value) pairs of PyObjectPtr instances, for
        the builtin variables
        '''
        if self.is_optimized_out():
            return ()

        pyop_builtins = self.pyop_field('f_builtins')
        return pyop_builtins.iteritems()

    def get_var_by_name(self, name):
        '''
        Look for the named local variable, returning a (PyObjectPtr, scope) pair
        where scope is a string 'local', 'global', 'builtin'

        If not found, return (None, None)
        '''
        for pyop_name, pyop_value in self.iter_locals():
            if name == pyop_name.proxyval(set()):
                return pyop_value, 'local'
        for pyop_name, pyop_value in self.iter_globals():
            if name == pyop_name.proxyval(set()):
                return pyop_value, 'global'
        for pyop_name, pyop_value in self.iter_builtins():
            if name == pyop_name.proxyval(set()):
                return pyop_value, 'builtin'
        return None, None

    def filename(self):
        '''Get the path of the current Python source file, as a string'''
        if self.is_optimized_out():
            return FRAME_INFO_OPTIMIZED_OUT
        return self.co_filename.proxyval(set())

    def current_line_num(self):
        '''Get current line number as an integer (1-based)

        Translated from PyFrame_GetLineNumber and PyCode_Addr2Line

        See Objects/lnotab_notes.txt
        '''
        if self.is_optimized_out():
            return None
        f_trace = self.field('f_trace')
        if long(f_trace) != 0:
            # we have a non-NULL f_trace:
            return self.f_lineno

        try:
            return self.co.addr2line(self.f_lasti)
        except Exception:
            # bpo-34989: addr2line() is a complex function, it can fail in many
            # ways. For example, it fails with a TypeError on "FakeRepr" if
            # gdb fails to load debug symbols. Use a catch-all "except
            # Exception" to make the whole function safe. The caller has to
            # handle None anyway for optimized Python.
            return None

    def current_line(self):
        '''Get the text of the current source line as a string, with a trailing
        newline character'''
        if self.is_optimized_out():
            return FRAME_INFO_OPTIMIZED_OUT

        lineno = self.current_line_num()
        if lineno is None:
            return '(failed to get frame line number)'

        filename = self.filename()
        try:
            with open(os_fsencode(filename), 'r') as fp:
                lines = fp.readlines()
        except IOError:
            return None

        try:
            # Convert from 1-based current_line_num to 0-based list offset
            return lines[lineno - 1]
        except IndexError:
            return None

    def write_repr(self, out, visited):
        if self.is_optimized_out():
            out.write(FRAME_INFO_OPTIMIZED_OUT)
            return
        lineno = self.current_line_num()
        lineno = str(lineno) if lineno is not None else "?"
        out.write('Frame 0x%x, for file %s, line %s, in %s ('
                  % (self.as_address(),
                     self.co_filename.proxyval(visited),
                     lineno,
                     self.co_name.proxyval(visited)))
        first = True
        for pyop_name, pyop_value in self.iter_locals():
            if not first:
                out.write(', ')
            first = False

            out.write(pyop_name.proxyval(visited))
            out.write('=')
            pyop_value.write_repr(out, visited)

        out.write(')')

    def print_traceback(self):
        if self.is_optimized_out():
            sys.stdout.write('  %s\n' % FRAME_INFO_OPTIMIZED_OUT)
            return
        visited = set()
        lineno = self.current_line_num()
        lineno = str(lineno) if lineno is not None else "?"
        sys.stdout.write('  File "%s", line %s, in %s\n'
                  % (self.co_filename.proxyval(visited),
                     lineno,
                     self.co_name.proxyval(visited)))

class PySetObjectPtr(PyObjectPtr):
    _typename = 'PySetObject'

    @classmethod
    def _dummy_key(self):
        return gdb.lookup_global_symbol('_PySet_Dummy').value()

    def __iter__(self):
        dummy_ptr = self._dummy_key()
        table = self.field('table')
        for i in safe_range(self.field('mask') + 1):
            setentry = table[i]
            key = setentry['key']
            if key != 0 and key != dummy_ptr:
                yield PyObjectPtr.from_pyobject_ptr(key)

    def proxyval(self, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('%s(...)' % self.safe_tp_name())
        visited.add(self.as_address())

        members = (key.proxyval(visited) for key in self)
        if self.safe_tp_name() == 'frozenset':
            return frozenset(members)
        else:
            return set(members)

    def write_repr(self, out, visited):
        # Emulate Python 3's set_repr
        tp_name = self.safe_tp_name()

        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('(...)')
            return
        visited.add(self.as_address())

        # Python 3's set_repr special-cases the empty set:
        if not self.field('used'):
            out.write(tp_name)
            out.write('()')
            return

        # Python 3 uses {} for set literals:
        if tp_name != 'set':
            out.write(tp_name)
            out.write('(')

        out.write('{')
        first = True
        for key in self:
            if not first:
                out.write(', ')
            first = False
            key.write_repr(out, visited)
        out.write('}')

        if tp_name != 'set':
            out.write(')')


class PyBytesObjectPtr(PyObjectPtr):
    _typename = 'PyBytesObject'

    def __str__(self):
        field_ob_size = self.field('ob_size')
        field_ob_sval = self.field('ob_sval')
        char_ptr = field_ob_sval.address.cast(_type_unsigned_char_ptr())
        return ''.join([chr(char_ptr[i]) for i in safe_range(field_ob_size)])

    def proxyval(self, visited):
        return str(self)

    def write_repr(self, out, visited):
        # Write this out as a Python 3 bytes literal, i.e. with a "b" prefix

        # Get a PyStringObject* within the Python 2 gdb process:
        proxy = self.proxyval(visited)

        # Transliteration of Python 3's Objects/bytesobject.c:PyBytes_Repr
        # to Python 2 code:
        quote = "'"
        if "'" in proxy and not '"' in proxy:
            quote = '"'
        out.write('b')
        out.write(quote)
        for byte in proxy:
            if byte == quote or byte == '\\':
                out.write('\\')
                out.write(byte)
            elif byte == '\t':
                out.write('\\t')
            elif byte == '\n':
                out.write('\\n')
            elif byte == '\r':
                out.write('\\r')
            elif byte < ' ' or ord(byte) >= 0x7f:
                out.write('\\x')
                out.write(hexdigits[(ord(byte) & 0xf0) >> 4])
                out.write(hexdigits[ord(byte) & 0xf])
            else:
                out.write(byte)
        out.write(quote)

class PyTupleObjectPtr(PyObjectPtr):
    _typename = 'PyTupleObject'

    def __getitem__(self, i):
        # Get the gdb.Value for the (PyObject*) with the given index:
        field_ob_item = self.field('ob_item')
        return field_ob_item[i]

    def proxyval(self, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            return ProxyAlreadyVisited('(...)')
        visited.add(self.as_address())

        result = tuple(PyObjectPtr.from_pyobject_ptr(self[i]).proxyval(visited)
                       for i in safe_range(int_from_int(self.field('ob_size'))))
        return result

    def write_repr(self, out, visited):
        # Guard against infinite loops:
        if self.as_address() in visited:
            out.write('(...)')
            return
        visited.add(self.as_address())

        out.write('(')
        for i in safe_range(int_from_int(self.field('ob_size'))):
            if i > 0:
                out.write(', ')
            element = PyObjectPtr.from_pyobject_ptr(self[i])
            element.write_repr(out, visited)
        if self.field('ob_size') == 1:
            out.write(',)')
        else:
            out.write(')')

class PyTypeObjectPtr(PyObjectPtr):
    _typename = 'PyTypeObject'


def _unichr_is_printable(char):
    # Logic adapted from Python 3's Tools/unicode/makeunicodedata.py
    if char == u" ":
        return True
    import unicodedata
    return unicodedata.category(char) not in ("C", "Z")

if sys.maxunicode >= 0x10000:
    _unichr = unichr
else:
    # Needed for proper surrogate support if sizeof(Py_UNICODE) is 2 in gdb
    def _unichr(x):
        if x < 0x10000:
            return unichr(x)
        x -= 0x10000
        ch1 = 0xD800 | (x >> 10)
        ch2 = 0xDC00 | (x & 0x3FF)
        return unichr(ch1) + unichr(ch2)


class PyUnicodeObjectPtr(PyObjectPtr):
    _typename = 'PyUnicodeObject'

    def char_width(self):
        _type_Py_UNICODE = gdb.lookup_type('Py_UNICODE')
        return _type_Py_UNICODE.sizeof

    def proxyval(self, visited):
        global _is_pep393
        if _is_pep393 is None:
            fields = gdb.lookup_type('PyUnicodeObject').fields()
            _is_pep393 = 'data' in [f.name for f in fields]
        if _is_pep393:
            # Python 3.3 and newer
            may_have_surrogates = False
            compact = self.field('_base')
            ascii = compact['_base']
            state = ascii['state']
            is_compact_ascii = (int(state['ascii']) and int(state['compact']))
            if not int(state['ready']):
                # string is not ready
                field_length = long(compact['wstr_length'])
                may_have_surrogates = True
                field_str = ascii['wstr']
            else:
                field_length = long(ascii['length'])
                if is_compact_ascii:
                    field_str = ascii.address + 1
                elif int(state['compact']):
                    field_str = compact.address + 1
                else:
                    field_str = self.field('data')['any']
                repr_kind = int(state['kind'])
                if repr_kind == 1:
                    field_str = field_str.cast(_type_unsigned_char_ptr())
                elif repr_kind == 2:
                    field_str = field_str.cast(_type_unsigned_short_ptr())
                elif repr_kind == 4:
                    field_str = field_str.cast(_type_unsigned_int_ptr())
        else:
            # Python 3.2 and earlier
            field_length = long(self.field('length'))
            field_str = self.field('str')
            may_have_surrogates = self.char_width() == 2

        # Gather a list of ints from the Py_UNICODE array; these are either
        # UCS-1, UCS-2 or UCS-4 code points:
        if not may_have_surrogates:
            Py_UNICODEs = [int(field_str[i]) for i in safe_range(field_length)]
        else:
            # A more elaborate routine if sizeof(Py_UNICODE) is 2 in the
            # inferior process: we must join surrogate pairs.
            Py_UNICODEs = []
            i = 0
            limit = safety_limit(field_length)
            while i < limit:
                ucs = int(field_str[i])
                i += 1
                if ucs < 0xD800 or ucs >= 0xDC00 or i == field_length:
                    Py_UNICODEs.append(ucs)
                    continue
                # This could be a surrogate pair.
                ucs2 = int(field_str[i])
                if ucs2 < 0xDC00 or ucs2 > 0xDFFF:
                    continue
                code = (ucs & 0x03FF) << 10
                code |= ucs2 & 0x03FF
                code += 0x00010000
                Py_UNICODEs.append(code)
                i += 1

        # Convert the int code points to unicode characters, and generate a
        # local unicode instance.
        # This splits surrogate pairs if sizeof(Py_UNICODE) is 2 here (in gdb).
        result = u''.join([
            (_unichr(ucs) if ucs <= 0x10ffff else '\ufffd')
            for ucs in Py_UNICODEs])
        return result

    def write_repr(self, out, visited):
        # Write this out as a Python 3 str literal, i.e. without a "u" prefix

        # Get a PyUnicodeObject* within the Python 2 gdb process:
        proxy = self.proxyval(visited)

        # Transliteration of Python 3's Object/unicodeobject.c:unicode_repr
        # to Python 2:
        if "'" in proxy and '"' not in proxy:
            quote = '"'
        else:
            quote = "'"
        out.write(quote)

        i = 0
        while i < len(proxy):
            ch = proxy[i]
            i += 1

            # Escape quotes and backslashes
            if ch == quote or ch == '\\':
                out.write('\\')
                out.write(ch)

            #  Map special whitespace to '\t', \n', '\r'
            elif ch == '\t':
                out.write('\\t')
            elif ch == '\n':
                out.write('\\n')
            elif ch == '\r':
                out.write('\\r')

            # Map non-printable US ASCII to '\xhh' */
            elif ch < ' ' or ch == 0x7F:
                out.write('\\x')
                out.write(hexdigits[(ord(ch) >> 4) & 0x000F])
                out.write(hexdigits[ord(ch) & 0x000F])

            # Copy ASCII characters as-is
            elif ord(ch) < 0x7F:
                out.write(ch)

            # Non-ASCII characters
            else:
                ucs = ch
                ch2 = None
                if sys.maxunicode < 0x10000:
                    # If sizeof(Py_UNICODE) is 2 here (in gdb), join
                    # surrogate pairs before calling _unichr_is_printable.
                    if (i < len(proxy)
                    and 0xD800 <= ord(ch) < 0xDC00 \
                    and 0xDC00 <= ord(proxy[i]) <= 0xDFFF):
                        ch2 = proxy[i]
                        ucs = ch + ch2
                        i += 1

                # Unfortuately, Python 2's unicode type doesn't seem
                # to expose the "isprintable" method
                printable = _unichr_is_printable(ucs)
                if printable:
                    try:
                        ucs.encode(ENCODING)
                    except UnicodeEncodeError:
                        printable = False

                # Map Unicode whitespace and control characters
                # (categories Z* and C* except ASCII space)
                if not printable:
                    if ch2 is not None:
                        # Match Python 3's representation of non-printable
                        # wide characters.
                        code = (ord(ch) & 0x03FF) << 10
                        code |= ord(ch2) & 0x03FF
                        code += 0x00010000
                    else:
                        code = ord(ucs)

                    # Map 8-bit characters to '\\xhh'
                    if code <= 0xff:
                        out.write('\\x')
                        out.write(hexdigits[(code >> 4) & 0x000F])
                        out.write(hexdigits[code & 0x000F])
                    # Map 21-bit characters to '\U00xxxxxx'
                    elif code >= 0x10000:
                        out.write('\\U')
                        out.write(hexdigits[(code >> 28) & 0x0000000F])
                        out.write(hexdigits[(code >> 24) & 0x0000000F])
                        out.write(hexdigits[(code >> 20) & 0x0000000F])
                        out.write(hexdigits[(code >> 16) & 0x0000000F])
                        out.write(hexdigits[(code >> 12) & 0x0000000F])
                        out.write(hexdigits[(code >> 8) & 0x0000000F])
                        out.write(hexdigits[(code >> 4) & 0x0000000F])
                        out.write(hexdigits[code & 0x0000000F])
                    # Map 16-bit characters to '\uxxxx'
                    else:
                        out.write('\\u')
                        out.write(hexdigits[(code >> 12) & 0x000F])
                        out.write(hexdigits[(code >> 8) & 0x000F])
                        out.write(hexdigits[(code >> 4) & 0x000F])
                        out.write(hexdigits[code & 0x000F])
                else:
                    # Copy characters as-is
                    out.write(ch)
                    if ch2 is not None:
                        out.write(ch2)

        out.write(quote)


class wrapperobject(PyObjectPtr):
    _typename = 'wrapperobject'

    def safe_name(self):
        try:
            name = self.field('descr')['d_base']['name'].string()
            return repr(name)
        except (NullPyObjectPtr, RuntimeError, UnicodeDecodeError):
            return '<unknown name>'

    def safe_tp_name(self):
        try:
            return self.field('self')['ob_type']['tp_name'].string()
        except (NullPyObjectPtr, RuntimeError, UnicodeDecodeError):
            return '<unknown tp_name>'

    def safe_self_addresss(self):
        try:
            address = long(self.field('self'))
            return '%#x' % address
        except (NullPyObjectPtr, RuntimeError):
            return '<failed to get self address>'

    def proxyval(self, visited):
        name = self.safe_name()
        tp_name = self.safe_tp_name()
        self_address = self.safe_self_addresss()
        return ("<method-wrapper %s of %s object at %s>"
                % (name, tp_name, self_address))

    def write_repr(self, out, visited):
        proxy = self.proxyval(visited)
        out.write(proxy)


def int_from_int(gdbval):
    return int(gdbval)


def stringify(val):
    # TODO: repr() puts everything on one line; pformat can be nicer, but
    # can lead to v.long results; this function isolates the choice
    if True:
        return repr(val)
    else:
        from pprint import pformat
        return pformat(val)


class PyObjectPtrPrinter:
    "Prints a (PyObject*)"

    def __init__ (self, gdbval):
        self.gdbval = gdbval

    def to_string (self):
        pyop = PyObjectPtr.from_pyobject_ptr(self.gdbval)
        if True:
            return pyop.get_truncated_repr(MAX_OUTPUT_LEN)
        else:
            # Generate full proxy value then stringify it.
            # Doing so could be expensive
            proxyval = pyop.proxyval(set())
            return stringify(proxyval)

def pretty_printer_lookup(gdbval):
    type = gdbval.type.unqualified()
    if type.code != gdb.TYPE_CODE_PTR:
        return None

    type = type.target().unqualified()
    t = str(type)
    if t in ("PyObject", "PyFrameObject", "PyUnicodeObject", "wrapperobject"):
        return PyObjectPtrPrinter(gdbval)

"""
During development, I've been manually invoking the code in this way:
(gdb) python

import sys
sys.path.append('/home/david/coding/python-gdb')
import libpython
end

then reloading it after each edit like this:
(gdb) python reload(libpython)

The following code should ensure that the prettyprinter is registered
if the code is autoloaded by gdb when visiting libpython.so, provided
that this python file is installed to the same path as the library (or its
.debug file) plus a "-gdb.py" suffix, e.g:
  /usr/lib/libpython2.6.so.1.0-gdb.py
  /usr/lib/debug/usr/lib/libpython2.6.so.1.0.debug-gdb.py
"""
def register (obj):
    if obj is None:
        obj = gdb

    # Wire up the pretty-printer
    obj.pretty_printers.append(pretty_printer_lookup)

register (gdb.current_objfile ())



# Unfortunately, the exact API exposed by the gdb module varies somewhat
# from build to build
# See http://bugs.python.org/issue8279?#msg102276

class Frame(object):
    '''
    Wrapper for gdb.Frame, adding various methods
    '''
    def __init__(self, gdbframe):
        self._gdbframe = gdbframe

    def older(self):
        older = self._gdbframe.older()
        if older:
            return Frame(older)
        else:
            return None

    def newer(self):
        newer = self._gdbframe.newer()
        if newer:
            return Frame(newer)
        else:
            return None

    def select(self):
        '''If supported, select this frame and return True; return False if unsupported

        Not all builds have a gdb.Frame.select method; seems to be present on Fedora 12
        onwards, but absent on Ubuntu buildbot'''
        if not hasattr(self._gdbframe, 'select'):
            print ('Unable to select frame: '
                   'this build of gdb does not expose a gdb.Frame.select method')
            return False
        self._gdbframe.select()
        return True

    def get_index(self):
        '''Calculate index of frame, starting at 0 for the newest frame within
        this thread'''
        index = 0
        # Go down until you reach the newest frame:
        iter_frame = self
        while iter_frame.newer():
            index += 1
            iter_frame = iter_frame.newer()
        return index

    # We divide frames into:
    #   - "python frames":
    #       - "bytecode frames" i.e. PyEval_EvalFrameEx
    #       - "other python frames": things that are of interest from a python
    #         POV, but aren't bytecode (e.g. GC, GIL)
    #   - everything else

    def is_python_frame(self):
        '''Is this a _PyEval_EvalFrameDefault frame, or some other important
        frame? (see is_other_python_frame for what "important" means in this
        context)'''
        if self.is_evalframe():
            return True
        if self.is_other_python_frame():
            return True
        return False

    def is_evalframe(self):
        '''Is this a _PyEval_EvalFrameDefault frame?'''
        if self._gdbframe.name() == EVALFRAME:
            '''
            I believe we also need to filter on the inline
            struct frame_id.inline_depth, only regarding frames with
            an inline depth of 0 as actually being this function

            So we reject those with type gdb.INLINE_FRAME
            '''
            if self._gdbframe.type() == gdb.NORMAL_FRAME:
                # We have a _PyEval_EvalFrameDefault frame:
                return True

        return False

    def is_other_python_frame(self):
        '''Is this frame worth displaying in python backtraces?
        Examples:
          - waiting on the GIL
          - garbage-collecting
          - within a CFunction
         If it is, return a descriptive string
         For other frames, return False
         '''
        if self.is_waiting_for_gil():
            return 'Waiting for the GIL'

        if self.is_gc_collect():
            return 'Garbage-collecting'

        # Detect invocations of PyCFunction instances:
        frame = self._gdbframe
        caller = frame.name()
        if not caller:
            return False

        if (caller.startswith('cfunction_vectorcall_') or
            caller == 'cfunction_call'):
            arg_name = 'func'
            # Within that frame:
            #   "func" is the local containing the PyObject* of the
            # PyCFunctionObject instance
            #   "f" is the same value, but cast to (PyCFunctionObject*)
            #   "self" is the (PyObject*) of the 'self'
            try:
                # Use the prettyprinter for the func:
                func = frame.read_var(arg_name)
                return str(func)
            except ValueError:
                return ('PyCFunction invocation (unable to read %s: '
                        'missing debuginfos?)' % arg_name)
            except RuntimeError:
                return 'PyCFunction invocation (unable to read %s)' % arg_name

        if caller == 'wrapper_call':
            arg_name = 'wp'
            try:
                func = frame.read_var(arg_name)
                return str(func)
            except ValueError:
                return ('<wrapper_call invocation (unable to read %s: '
                        'missing debuginfos?)>' % arg_name)
            except RuntimeError:
                return '<wrapper_call invocation (unable to read %s)>' % arg_name

        # This frame isn't worth reporting:
        return False

    def is_waiting_for_gil(self):
        '''Is this frame waiting on the GIL?'''
        # This assumes the _POSIX_THREADS version of Python/ceval_gil.h:
        name = self._gdbframe.name()
        if name:
            return (name == 'take_gil')

    def is_gc_collect(self):
        '''Is this frame gc_collect_main() within the garbage-collector?'''
        return self._gdbframe.name() in ('collect', 'gc_collect_main')

    def get_pyop(self):
        try:
            f = self._gdbframe.read_var('f')
            frame = PyFrameObjectPtr.from_pyobject_ptr(f)
            if not frame.is_optimized_out():
                return frame
            # gdb is unable to get the "f" argument of PyEval_EvalFrameEx()
            # because it was "optimized out". Try to get "f" from the frame
            # of the caller, PyEval_EvalCodeEx().
            orig_frame = frame
            caller = self._gdbframe.older()
            if caller:
                f = caller.read_var('f')
                frame = PyFrameObjectPtr.from_pyobject_ptr(f)
                if not frame.is_optimized_out():
                    return frame
            return orig_frame
        except ValueError:
            return None

    @classmethod
    def get_selected_frame(cls):
        _gdbframe = gdb.selected_frame()
        if _gdbframe:
            return Frame(_gdbframe)
        return None

    @classmethod
    def get_selected_python_frame(cls):
        '''Try to obtain the Frame for the python-related code in the selected
        frame, or None'''
        try:
            frame = cls.get_selected_frame()
        except gdb.error:
            # No frame: Python didn't start yet
            return None

        while frame:
            if frame.is_python_frame():
                return frame
            frame = frame.older()

        # Not found:
        return None

    @classmethod
    def get_selected_bytecode_frame(cls):
        '''Try to obtain the Frame for the python bytecode interpreter in the
        selected GDB frame, or None'''
        frame = cls.get_selected_frame()

        while frame:
            if frame.is_evalframe():
                return frame
            frame = frame.older()

        # Not found:
        return None

    def print_summary(self):
        if self.is_evalframe():
            pyop = self.get_pyop()
            if pyop:
                line = pyop.get_truncated_repr(MAX_OUTPUT_LEN)
                write_unicode(sys.stdout, '#%i %s\n' % (self.get_index(), line))
                if not pyop.is_optimized_out():
                    line = pyop.current_line()
                    if line is not None:
                        sys.stdout.write('    %s\n' % line.strip())
            else:
                sys.stdout.write('#%i (unable to read python frame information)\n' % self.get_index())
        else:
            info = self.is_other_python_frame()
            if info:
                sys.stdout.write('#%i %s\n' % (self.get_index(), info))
            else:
                sys.stdout.write('#%i\n' % self.get_index())

    def print_traceback(self):
        if self.is_evalframe():
            pyop = self.get_pyop()
            if pyop:
                pyop.print_traceback()
                if not pyop.is_optimized_out():
                    line = pyop.current_line()
                    if line is not None:
                        sys.stdout.write('    %s\n' % line.strip())
            else:
                sys.stdout.write('  (unable to read python frame information)\n')
        else:
            info = self.is_other_python_frame()
            if info:
                sys.stdout.write('  %s\n' % info)
            else:
                sys.stdout.write('  (not a python frame)\n')

class PyList(gdb.Command):
    '''List the current Python source code, if any

    Use
       py-list START
    to list at a different line number within the python source.

    Use
       py-list START, END
    to list a specific range of lines within the python source.
    '''

    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-list",
                              gdb.COMMAND_FILES,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        import re

        start = None
        end = None

        m = re.match(r'\s*(\d+)\s*', args)
        if m:
            start = int(m.group(0))
            end = start + 10

        m = re.match(r'\s*(\d+)\s*,\s*(\d+)\s*', args)
        if m:
            start, end = map(int, m.groups())

        # py-list requires an actual PyEval_EvalFrameEx frame:
        frame = Frame.get_selected_bytecode_frame()
        if not frame:
            print('Unable to locate gdb frame for python bytecode interpreter')
            return

        pyop = frame.get_pyop()
        if not pyop or pyop.is_optimized_out():
            print(UNABLE_READ_INFO_PYTHON_FRAME)
            return

        filename = pyop.filename()
        lineno = pyop.current_line_num()
        if lineno is None:
            print('Unable to read python frame line number')
            return

        if start is None:
            start = lineno - 5
            end = lineno + 5

        if start<1:
            start = 1

        try:
            f = open(os_fsencode(filename), 'r')
        except IOError as err:
            sys.stdout.write('Unable to open %s: %s\n'
                             % (filename, err))
            return
        with f:
            all_lines = f.readlines()
            # start and end are 1-based, all_lines is 0-based;
            # so [start-1:end] as a python slice gives us [start, end] as a
            # closed interval
            for i, line in enumerate(all_lines[start-1:end]):
                linestr = str(i+start)
                # Highlight current line:
                if i + start == lineno:
                    linestr = '>' + linestr
                sys.stdout.write('%4s    %s' % (linestr, line))


# ...and register the command:
PyList()

def move_in_stack(move_up):
    '''Move up or down the stack (for the py-up/py-down command)'''
    frame = Frame.get_selected_python_frame()
    if not frame:
        print('Unable to locate python frame')
        return

    while frame:
        if move_up:
            iter_frame = frame.older()
        else:
            iter_frame = frame.newer()

        if not iter_frame:
            break

        if iter_frame.is_python_frame():
            # Result:
            if iter_frame.select():
                iter_frame.print_summary()
            return

        frame = iter_frame

    if move_up:
        print('Unable to find an older python frame')
    else:
        print('Unable to find a newer python frame')

class PyUp(gdb.Command):
    'Select and print the python stack frame that called this one (if any)'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-up",
                              gdb.COMMAND_STACK,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        move_in_stack(move_up=True)

class PyDown(gdb.Command):
    'Select and print the python stack frame called by this one (if any)'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-down",
                              gdb.COMMAND_STACK,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        move_in_stack(move_up=False)

# Not all builds of gdb have gdb.Frame.select
if hasattr(gdb.Frame, 'select'):
    PyUp()
    PyDown()

class PyBacktraceFull(gdb.Command):
    'Display the current python frame and all the frames within its call stack (if any)'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-bt-full",
                              gdb.COMMAND_STACK,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        frame = Frame.get_selected_python_frame()
        if not frame:
            print('Unable to locate python frame')
            return

        while frame:
            if frame.is_python_frame():
                frame.print_summary()
            frame = frame.older()

PyBacktraceFull()

class PyBacktrace(gdb.Command):
    'Display the current python frame and all the frames within its call stack (if any)'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-bt",
                              gdb.COMMAND_STACK,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        frame = Frame.get_selected_python_frame()
        if not frame:
            print('Unable to locate python frame')
            return

        sys.stdout.write('Traceback (most recent call first):\n')
        while frame:
            if frame.is_python_frame():
                frame.print_traceback()
            frame = frame.older()

PyBacktrace()

class PyPrint(gdb.Command):
    'Look up the given python variable name, and print it'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-print",
                              gdb.COMMAND_DATA,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        name = str(args)

        frame = Frame.get_selected_python_frame()
        if not frame:
            print('Unable to locate python frame')
            return

        pyop_frame = frame.get_pyop()
        if not pyop_frame:
            print(UNABLE_READ_INFO_PYTHON_FRAME)
            return

        pyop_var, scope = pyop_frame.get_var_by_name(name)

        if pyop_var:
            print('%s %r = %s'
                   % (scope,
                      name,
                      pyop_var.get_truncated_repr(MAX_OUTPUT_LEN)))
        else:
            print('%r not found' % name)

PyPrint()

class PyLocals(gdb.Command):
    'Look up the given python variable name, and print it'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-locals",
                              gdb.COMMAND_DATA,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        name = str(args)

        frame = Frame.get_selected_python_frame()
        if not frame:
            print('Unable to locate python frame')
            return

        pyop_frame = frame.get_pyop()
        if not pyop_frame:
            print(UNABLE_READ_INFO_PYTHON_FRAME)
            return

        for pyop_name, pyop_value in pyop_frame.iter_locals():
            print('%s = %s'
                   % (pyop_name.proxyval(set()),
                      pyop_value.get_truncated_repr(MAX_OUTPUT_LEN)))

PyLocals()


import ast
from dataclasses import dataclass
from enum import Enum
import re
from typing import Optional

class TokenType(Enum):
    HISTORY = "HISTORY"
    CONVENIENCE = "CONVENIENCE"
    NAME = "NAME"
    STRING = "STRING"
    NUM = "NUM"
    OPEN_BRACKET = "OPEN_BRACKET"
    CLOSE_BRACKET = "CLOSE_BRACKET"
    OPEN_PAREN = "OPEN_PAREN"
    CLOSE_PAREN = "CLOSE_PAREN"
    COMMA = "COMMA"
    DOT = "DOT"

class OpType(Enum):
    # Integration with GDB
    GDB_HISTORY = 'GDB_HISTORY'
    GDB_CONVENIENCE = 'GDB_CONVENIENCE'
    # Python's valies
    PY_LOAD_NAME = 'PY_LOAD_NAME'
    PY_LOAD_ATTR = 'PY_LOAD_ATTR'
    PY_BINARY_SUBSCR = 'PY_BINARY_SUBSCR'
    # Helpers for scripting
    PUSH = 'PUSH'
    BUILD_TUPLE = 'BUILD_TUPLE'
    # Helpers for completion
    OPEN_CTX = 'OPEN_CTX'
    CLOSE_CTX = 'CLOSE_CTX'

@dataclass
class ParseError:
    code: str
    token: Optional[Token]
    pos: int
    msg: str

@dataclass
class Token:
    token_type: TokenType
    value: str
    end: int

@dataclass
class Op:
    op_type: OpType
    token: Token
    value: object

class PyEvaluateParser:
    """ Implements a DSL to interact with python values from the GDB prompt.
    This will accept expressions like:
    - `self.attr['key']`
    - `$1.attr['key']` where `$1` is a GDB history value which points to PyObject*
    - `$1.$attr['key']` where `$1` is a GDB history and `$attr` a convenience
      variable, both values point to PyObject*
    """
    def __init__(self):
        # > These are successive integers starting with one
        # https://sourceware.org/gdb/current/onlinedocs/gdb/Value-History.html#Value-History
        self.HISTORY = re.compile(r'[$][1-9][0-9]*')
        # > Any name preceded by ‘$’ can be used for a convenience variable,
        # unless it is one of the predefined machine-specific register names
        # https://sourceware.org/gdb/current/onlinedocs/gdb/Convenience-Vars.html#Convenience-Vars
        self.CONVENIENCE = re.compile(r'[$][_a-zA-Z][_a-zA-Z0-9]*')
        # This only supports ascii characters :(
        # https://docs.python.org/3/reference/lexical_analysis.html#identifiers
        self.NAME = re.compile(r'[_a-zA-Z][_a-zA-Z0-9]*')
        self.SPACE = re.compile(r'\s+')
        self.NUM = re.compile(r'\d+([.]\d*)?')
        # This does not handle escaped quotes :(
        self.STRING = re.compile(r'"[^"]*"|\'[^\']*\'')
        self.char_to_type = {
            '.': TokenType.DOT,
            ',': TokenType.COMMA,
            '(': TokenType.OPEN_PAREN,
            ')': TokenType.CLOSE_PAREN,
            '[': TokenType.OPEN_BRACKET,
            ']': TokenType.CLOSE_BRACKET,
        }
    def parser_error(self, code, token, pos, expected):
        if token is not None:
            line = '~' * (token.end - 1) + '^'
            msg = '\n'.join(['', code, line, f"Got token {token}, expected {expected}"])
        else:
            line = '~' * (len(code) - 1) + '^'
            msg = '\n'.join(['', code, line, f"Code ended abruptly, expecting {expected}"])
        return ParseError(code, token, pos, msg)
    def assert_no_extra(self, code, token, pos):
        extra = code[token.end:].strip()
        if extra:
            line = '~' * token.end + '^'
            msg = '\n'.join(['', code, line, f"Unrecognized code"])
            return self.parser_error(code, token, pos, msg)
    def next_token(self, code, pos):
        """ Returns the token at position `pos` in `code`.
        Note: This does not consume the string, calling it is equivalent to peeking.
        """
        start = pos
        to_match = code[start:]
        match = self.SPACE.match(to_match)
        if match:
            start = pos + match.endpos
            to_match = code[start:]
        if not to_match:
            return
        history = self.HISTORY.match(to_match)
        if history:
            end = history.end()
            return Token(TokenType.HISTORY, to_match[:end], pos + end)
        convenience = self.CONVENIENCE.match(to_match)
        if convenience:
            end = convenience.end()
            return Token(TokenType.CONVENIENCE, to_match[:end], pos + end)
        num = self.NUM.match(to_match)
        if num:
            end = num.end()
            return Token(TokenType.NUM, to_match[:end], pos + end)
        string = self.STRING.match(to_match)
        if string:
            end = string.end()
            return Token(TokenType.STRING, to_match[:end], pos + end)
        name = self.NAME.match(to_match)
        if name:
            end = name.end()
            return Token(TokenType.NAME, to_match[:end], pos + end)
        token_type = self.char_to_type.get(to_match[0])
        if token_type:
            return Token(token_type, to_match[0], pos + 1)
    def parse_ctx(self, code, pos):
        if not code:
            return
        token = self.next_token(code, pos)
        if not token:
            yield self.parser_error(code, token, pos, 'name')
            return
        if token.token_type is TokenType.HISTORY:
            yield Op(OpType.OPEN_CTX, token, None)
            yield Op(OpType.GDB_HISTORY, token, int(token.value[1:]))
        elif token.token_type is TokenType.CONVENIENCE:
            yield Op(OpType.OPEN_CTX, token, None)
            yield Op(OpType.GDB_CONVENIENCE, token, token.value[1:])
        elif token.token_type is TokenType.NAME:
            yield Op(OpType.OPEN_CTX, token, None)
            yield Op(OpType.PY_LOAD_NAME, token, token.value)
        else:
            yield self.parser_error(code, token, pos, 'name')
            return
        if len(code) > token.end:
            for op in self.parse_accessor(code, token.end):
                yield op
                if isinstance(op, ParseError):
                    return
            error = self.assert_no_extra(code, op.token, op.token.end)
            if error:
                yield error
                return
        yield Op(OpType.CLOSE_CTX, token, None)
    def parse_accessor(self, code, pos):
        accessor_token = self.next_token(code, pos)
        while accessor_token is not None:
            if accessor_token.token_type is TokenType.OPEN_BRACKET:
                for op in self.parse_bracket(code, pos):
                    yield op
                    if isinstance(op, ParseError):
                        return
                pos = op.token.end
            elif accessor_token.token_type is TokenType.DOT:
                token = self.next_token(code, accessor_token.end)
                if token and token.token_type is TokenType.HISTORY:
                    yield Op(OpType.GDB_HISTORY, token, int(token.value[1:]))
                elif token and token.token_type is TokenType.CONVENIENCE:
                    yield Op(OpType.GDB_CONVENIENCE, token, token.value[1:])
                elif token and token.token_type is TokenType.NAME:
                    yield Op(OpType.PUSH, token, token.value)
                else:
                    yield self.parser_error(code, token, pos, 'name')
                    return
                yield Op(OpType.PY_LOAD_ATTR, token, token.value)
                pos = token.end
            else:
                yield self.parser_error(code, token, pos, '. or [')
                return
            accessor_token = self.next_token(code, pos)
    def parse_bracket(self, code, pos):
        token = self.next_token(code, pos)
        assert token.token_type is TokenType.OPEN_BRACKET, 'parse_bracket has to be called pointing to a ['
        token = self.next_token(code, token.end)
        if token and token.token_type in (TokenType.HISTORY, TokenType.CONVENIENCE, TokenType.NAME):
            subops = self.parse_ctx(code, token.end)
        elif token and token.token_type is TokenType.OPEN_PAREN:
            subops = self.parse_paren(code, pos)
        elif token and token.token_type is TokenType.STRING:
            subops = [Op(OpType.PUSH, token, token.value[1:-1])]
        elif token and token.token_type is TokenType.NUM:
            value = token.value
            value = float(value) if '.' in value else int(value)
            subops = [Op(OpType.PUSH, token, value)]
        else:
            yield self.parser_error(code, token, pos, 'name, string, number or (')
            return
        for op in subops:
            yield op
            if isinstance(op, ParseError):
                return
        token = self.next_token(code, op.token.end)
        if token is None or token.token_type is not TokenType.CLOSE_BRACKET:
            yield self.parser_error(code, token, pos, ']')
            return
        yield Op(OpType.PY_BINARY_SUBSCR, token, None)
    def parse_paren(self, code, pos):
        token = self.next_token(code, pos)
        assert token.token_type is TokenType.OPEN_PAREN, 'parse_paren has to be called pointing to a ('
        token = self.next_token(code, token.end)
        qty_elements = 0
        while token and token.token_type is not TokenType.CLOSE_PAREN:
            for op in self.parse_ctx(code, token.end):
                yield op
                if isinstance(op, ParseError):
                    return
            qty_elements += 1
            new_pos = op.token.end
            token = self.next_token(code, new_pos)
            if token is None or token.token_type not in (TokenType.CLOSE_BRACKET, TokenType.COMMA):
                yield self.parser_error(code, token, new_pos, ', or )')
                return
            token = self.next_token(code, token.end)
        if token is None or token.token_type is not TokenType.CLOSE_PAREN:
            yield self.parser_error(code, token, pos, ', or )')
            return
        yield Op(OpType.BUILD_TUPLE, token, qty_elements)

class PyEvaluateInterpreter:
    def evaluate(self, ops, pyop_frame):
        stack = []
        for op in ops:
            if op.op_type is OpType.GDB_HISTORY:
                self.gdb_history(stack, op.value)
            elif op.op_type is OpType.GDB_CONVENIENCE:
                self.gdb_convenience(stack, op.value)
            elif op.op_type is OpType.PY_LOAD_NAME:
                self.load_name(stack, pyop_frame, op.value)
            elif op.op_type is OpType.PY_LOAD_ATTR:
                self.load_attr(stack)
            elif op.op_type is OpType.PY_BINARY_SUBSCR:
                self.binary_subscr(stack)
            elif op.op_type is OpType.PUSH:
                self.push(stack, op.value)
            elif op.op_type is OpType.BUILD_TUPLE:
                self.build_tuple(stack, op.token, op.value)
            elif isinstance(op, ParseError):
                raise RuntimeError('Cannot evaluate expressions with parsing errors.')
        return stack
    def get_matching_key(self, d, k):
        for key, value in d.iteritems():
            if key.proxyval(set()) == k:
                return value
    def gdb_convenience(self, stack, name):
        value = gdb.convenience_variable(name)
        stack.append(PyObjectPtr.from_pyobject_ptr(value))
    def gdb_history(self, stack, name):
        value = gdb.history(name)
        stack.append(PyObjectPtr.from_pyobject_ptr(value))
    def load_name(self, stack, pyop_frame, name):
        pyop_var, _ = pyop_frame.get_var_by_name(name)
        stack.append(pyop_var)
    def load_attr(self, stack):
        name = stack.pop()
        owner = stack.pop()
        if isinstance(owner, HeapTypeObjectPtr):
            attr_dict = owner.get_attr_dict()
            if attr_dict:
                value = self.get_matching_key(attr_dict, name)
                if value is None:
                    tp_name = owner.type().field('tp_name').string()
                    raise AttributeError(f"'{tp_name}' object has no name '{name}'")
                else:
                    stack.append(value)
        else:
            raise RuntimeError('Objects with a custom getattr protocol are not supported.')
    def binary_subscr(self, stack):
        sub = stack.pop()
        container = stack.pop()
        if isinstance(container, PyDictObjectPtr):
            value = self.get_matching_key(container, sub)
            if value is None:
                raise KeyError(f"KeyError: {sub}")
            stack.append(value)
        elif isinstance(container, (PyTupleObjectPtr, PyListObjectPtr)):
            stack.append(container[sub])
        else:
            raise KeyError(f"TypeError: {type(container)}")
    def push(self, stack, value):
        stack.append(value)
    def build_tuple(self, stack, value):
        t = tuple(stack[-value:])
        for _ in range(value):
            stack.pop()
        stack.append(t)

class PyEvaluate(gdb.Command):
    CMD_NAME = "py-evaluate"
    def __init__(self):
        gdb.Command.__init__(self, self.CMD_NAME, gdb.COMMAND_DATA)
        self._parser = PyEvaluateParser()
        self._interpreter = PyEvaluateInterpreter()
    def get_frame(self):
        frame = Frame.get_selected_python_frame()
        if not frame:
            return
        pyop_frame = frame.get_pyop()
        if not pyop_frame:
            return
        return pyop_frame
    def invoke(self, text_args, from_tty):
        args = text_args.split()
        if len(args) > 2 or len(args) < 1:
            print(f'{self.CMD_NAME} EXPR <MAX_OUTPUT_LEN>')
            return
        output_length = MAX_OUTPUT_LEN
        if len(args) == 2:
            output_length = int(args[1])
        expr = args[0]
        pyop_frame = self.get_frame()
        if pyop_frame is None:
            print('Unable to locate python frame')
            return
        ops = list(self._parser.parse_ctx(expr, 0))
        if not ops:
            return
        elif isinstance(ops[-1], ParseError):
            raise SyntaxError(ops[-1].value)
        stack = self._interpreter.evaluate(ops, pyop_frame)
        assert len(stack) == 1, f'Stack should end up with a single element! {stack}'
        result = stack[0]
        print('%s' % result.get_truncated_repr(output_length))
        # Executing the `print` command to save the Value in GDB's value
        # history.
        gdb.execute(f'print ({result._typename}*){result.as_address()}')
    def _find_ctx(self, ops):
        last_el = len(ops) - 1
        # skip the parsing error token
        start = last_el - 1
        qty_closed_ctx = 0
        for pos in range(start, -1, -1):
            op = ops[pos]
            if op.op_type is OpType.CLOSE_CTX:
                qty_closed_ctx += 1
            elif op.op_type is OpType.OPEN_CTX:
                if qty_closed_ctx == 0:
                    break
                qty_closed_ctx -= 1
        assert qty_closed_ctx == 0, 'Non empty sequence of ops must have OPEN_CTX'
        assert ops[pos].op_type is OpType.OPEN_CTX, 'Non empty sequence of ops must have OPEN_CTX'
        return ops[pos:]
    def _get_stack_and_op_to_complete(self, ops, pyop_frame):
        if not ops:
            return None, None
        if isinstance(ops[-1], ParseError):
            ops = self._find_ctx(ops)
            op_to_complete = ops[-1]
            ops = ops[:-1]
        else:
            assert ops[-1].op_type is OpType.CLOSE_CTX, 'Sucessfully parsed expressions must end with a CLOSE_CTX.'
            op_to_complete = ops[-2]
            ops = ops[:-2]
        # the first entry of ops is a OPEN_CTX, which will fail if evaluated alone
        if len(ops) > 1:
            stack = self._interpreter.evaluate(ops, pyop_frame)
            return stack, op_to_complete
        return None, op_to_complete
    def _names_for_prompt(self, op_to_complete, pyop_frame):
        names_locals = [pyop_name.proxyval(set()) for pyop_name, _ in pyop_frame.iter_locals() or []]
        names_globals = [pyop_name.proxyval(set()) for pyop_name, _ in pyop_frame.iter_globals() or []]
        names_builtins = [pyop_name.proxyval(set()) for pyop_name, _ in pyop_frame.iter_builtins() or []]
        all_names = names_locals + names_globals + names_globals
        if op_to_complete:
            # Cannot complete history and convenience variables
            assert op_to_complete.op_type is OpType.PY_LOAD_NAME, "Other ops can not be completed from top-level"
            all_names = [
                name
                for name in all_names
                if name.startswith(op_to_complete.value) and name != op_to_complete.value
            ]
        return all_names
    def _names_for_pyobject(self, pyobj):
        if isinstance(pyobj, HeapTypeObjectPtr):
            attribute_dict = pyobj.get_attr_dict()
            all_names = [
                key.proxyval(set())
                for key, _ in attribute_dict.iteritems()
            ]
        else:
            proxy = pyobj.proxyval(set())
            all_names = dir(proxy)
        return all_names
    def _names_for_load_attr(self, stack, op_to_complete):
        assert len(stack) == 2, "The stack should have the PyObject* and the partial identifier"
        assert op_to_complete.op_type is OpType.PY_LOAD_ATTR
        pyobj = stack[0]
        all_names = self._names_for_pyobject(pyobj)
        filtered_names = [
            name
            for name in all_names
            if name.startswith(op_to_complete.value) and name != op_to_complete.value
        ]
        return filtered_names
    def _names_for_incomplete_expr(self, stack, parser_error):
        assert len(stack) == 1, "The stack should have only the PyObject*"
        assert isinstance(parser_error, ParseError)
        pyobj = stack[0]
        if parser_error.code[parser_error.pos] == '.':
            all_names = self._names_for_pyobject(pyobj)
            return all_names
        if parser_error.code[parser_error.pos] == '[':
            all_names = self._keys_for_pyobject(pyobj)
            return all_names
    def complete(self, text, word):
        args = text.split()
        # only the first arg can be autocompleted
        if " " in text:
            return []
        elif len(args) == 1:
            expr = args[0]
        else:
            expr = ""
        ops = list(self._parser.parse_ctx(expr, 0))
        pyop_frame = self.get_frame()
        stack, op_to_complete = self._get_stack_and_op_to_complete(ops, pyop_frame)
        if stack and isinstance(op_to_complete, ParseError):
            names = self._names_for_incomplete_expr(stack, op_to_complete)
        elif stack and op_to_complete and op_to_complete.op_type is OpType.PY_LOAD_ATTR:
            names = self._names_for_load_attr(stack, op_to_complete)
        elif op_to_complete is None or op_to_complete.op_type is OpType.PY_LOAD_NAME:
            names = self._names_for_prompt(op_to_complete, pyop_frame)
        else:
            # Either a parser error without a ctx or history/convenience
            # variables, either of which can not be autocompleted
            names = []
        return [n.strip() for n in names]

PyEvaluate()
