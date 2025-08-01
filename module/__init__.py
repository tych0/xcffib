# Copyright 2014 Tycho Andersen
# Copyright 2014 Sean Vig
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division

import ctypes.util
import functools
import io
import platform
import struct
import weakref

from .ffi import ffi

if platform.system() == "Darwin":
    soname = "libxcb.dylib"
elif platform.system() == "Windows":
    soname = "libxcb.dll"
else:
    soname = ctypes.util.find_library("xcb")
    if soname is None:
        soname = "libxcb.so"
lib = ffi.dlopen(soname)

# Add this for portable access to C's free()
c_lib = ffi.dlopen(None)  # Loads the global symbol table (works on Unix/BSD)
c_free = c_lib.free

__xcb_proto_version__ = "placeholder"
__version__ = "placeholder"

X_PROTOCOL = lib.X_PROTOCOL
X_PROTOCOL_REVISION = lib.X_PROTOCOL_REVISION

XCB_NONE = lib.XCB_NONE
XCB_COPY_FROM_PARENT = lib.XCB_COPY_FROM_PARENT
XCB_CURRENT_TIME = lib.XCB_CURRENT_TIME
XCB_NO_SYMBOL = lib.XCB_NO_SYMBOL

# For xpyb compatibility
NONE = XCB_NONE
CopyFromParent = XCB_COPY_FROM_PARENT
CurrentTime = XCB_CURRENT_TIME
NoSymbol = XCB_NO_SYMBOL

XCB_CONN_ERROR = lib.XCB_CONN_ERROR
XCB_CONN_CLOSED_EXT_NOTSUPPORTED = lib.XCB_CONN_CLOSED_EXT_NOTSUPPORTED
XCB_CONN_CLOSED_MEM_INSUFFICIENT = lib.XCB_CONN_CLOSED_MEM_INSUFFICIENT
XCB_CONN_CLOSED_REQ_LEN_EXCEED = lib.XCB_CONN_CLOSED_REQ_LEN_EXCEED
XCB_CONN_CLOSED_PARSE_ERR = lib.XCB_CONN_CLOSED_PARSE_ERR
XCB_CONN_CLOSED_INVALID_SCREEN = lib.XCB_CONN_CLOSED_INVALID_SCREEN
XCB_CONN_CLOSED_FDPASSING_FAILED = lib.XCB_CONN_CLOSED_FDPASSING_FAILED

cffi_explicit_lifetimes = weakref.WeakKeyDictionary()


def type_pad(t, i):
    return -i & (3 if t > 4 else t - 1)


def visualtype_to_c_struct(vt):
    # let ffi be a kwarg so cairocffi can pass in its ffi
    # cfficairo needs an xcb_visualtype_t
    s = ffi.new("struct xcb_visualtype_t *")

    s.visual_id = vt.visual_id
    s._class = vt._class
    s.bits_per_rgb_value = vt.bits_per_rgb_value
    s.colormap_entries = vt.colormap_entries
    s.red_mask = vt.red_mask
    s.green_mask = vt.green_mask
    s.blue_mask = vt.blue_mask

    return s


class Unpacker(object):
    def __init__(self, known_max=None):
        self.size = 0
        self.offset = 0
        self.known_max = known_max
        if self.known_max is not None:
            self._resize(known_max)

    def pad(self, thing):
        if isinstance(thing, type) and issubclass(thing, (Struct, Union)):
            if hasattr(thing, "fixed_size"):
                size = thing.fixed_size
            else:
                size = 4
        else:
            size = struct.calcsize(thing)

        self.offset += type_pad(size, self.offset)

    def unpack(self, fmt, increment=True):
        fmt = "=" + fmt
        size = struct.calcsize(fmt)
        if size > self.size - self.offset:
            self._resize(size)
        ret = struct.unpack_from(fmt, self.buf, self.offset)

        if increment:
            self.offset += size
        return ret

    def cast(self, typ):
        assert self.offset == 0
        return ffi.cast(typ, self.cdata)

    def copy(self):
        raise NotImplementedError

    @classmethod
    def synthetic(cls, data, format):
        self = cls.__new__(cls)
        self.__init__(len(data))
        self.buf = data
        self.offset = 0
        self.size = len(data)
        return self


class CffiUnpacker(Unpacker):
    def __init__(self, cdata, known_max=None):
        self.cdata = cdata
        Unpacker.__init__(self, known_max)

    def _resize(self, increment):
        if self.offset + increment > self.size:
            if self.known_max is not None:
                assert self.size + increment <= self.known_max
            self.size = self.offset + increment
            self.buf = ffi.buffer(self.cdata, self.size)

    def copy(self):
        new = CffiUnpacker(self.cdata, self.known_max)
        new.offset = self.offset
        new.size = self.size
        return new


class MemoryUnpacker(Unpacker):
    def __init__(self, buf):
        self.buf = buf
        Unpacker.__init__(self, len(self.buf))

    def _resize(self, increment):
        if self.size + increment > self.known_max:
            raise XcffibException("resizing memory buffer to be too big")
        self.size += increment

    def copy(self):
        new = MemoryUnpacker(self.buf)
        new.offset = self.offset
        new.size = self.size
        return new


def popcount(n):
    return bin(n).count("1")


class XcffibException(Exception):
    """Generic XcbException; replaces xcb.Exception."""

    pass


class XcffibNotImplemented(XcffibException, NotImplementedError):
    pass


class ConnectionException(XcffibException):
    REASONS = {
        lib.XCB_CONN_ERROR: (
            "xcb connection errors because of socket, pipe and other stream errors."
        ),
        lib.XCB_CONN_CLOSED_EXT_NOTSUPPORTED: (
            "xcb connection shutdown because extension not supported"
        ),
        lib.XCB_CONN_CLOSED_MEM_INSUFFICIENT: (
            "malloc(), calloc() and realloc() error upon failure, for eg ENOMEM"
        ),
        lib.XCB_CONN_CLOSED_REQ_LEN_EXCEED: (
            "Connection closed, exceeding request length that server accepts."
        ),
        lib.XCB_CONN_CLOSED_PARSE_ERR: (
            "Connection closed, error during parsing display string."
        ),
        lib.XCB_CONN_CLOSED_INVALID_SCREEN: (
            "Connection closed because the server does not have a screen "
            "matching the display."
        ),
        lib.XCB_CONN_CLOSED_FDPASSING_FAILED: (
            "Connection closed because some FD passing operation failed"
        ),
    }

    def __init__(self, err):
        XcffibException.__init__(
            self, self.REASONS.get(err, "Unknown connection error.")
        )


class ProtocolException(XcffibException):
    pass


core = None
core_events = None
core_errors = None
# we use _setup here instead of just setup because of a nose bug that triggers
# when doing the packaging builds in debian:
# https://code.google.com/p/python-nose/issues/detail?id=326
_setup = None

extensions = {}

# This seems a bit over engineered to me; it seems unlikely there will ever be
# a core besides xproto, so why not just hardcode that?


def _add_core(value, __setup, events, errors):
    if not issubclass(value, Extension):
        raise XcffibException("Extension type not derived from xcffib.Extension")
    if not issubclass(__setup, Struct):
        raise XcffibException("Setup type not derived from xcffib.Struct")

    global core
    global core_events
    global core_errors
    global _setup

    core = value
    core_events = events
    core_errors = errors
    _setup = __setup


def _add_ext(key, value, events, errors):
    if not issubclass(value, Extension):
        raise XcffibException("Extension type not derived from xcffib.Extension")
    extensions[key] = (value, events, errors)


class ExtensionKey(object):
    def __init__(self, name):
        self.name = name
        self.c_key = ffi.new("struct xcb_extension_t *")
        self.c_name = ffi.new("char[]", self.name.encode())
        self.c_key.name = self.c_name
        self.c_key.global_id = 0

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, o):
        return self.name == o.name

    def __ne__(self, o):
        return self.name != o.name


class Protobj(object):
    """Note: Unlike xcb.Protobj, this does NOT implement the sequence
    protocol. I found this behavior confusing: Protobj would implement the
    sequence protocol on self.buf, and then List would go and implement it on
    List.

    Instead, when we need to create a new event from an existing event, we
    repack that event into a MemoryUnpacker and use that instead (see
    eventToUnpacker in the generator for more info.)
    """

    def __init__(self, unpacker):
        """
        Params:
        - unpacker: an Unpacker object
        """

        # if we don't know the size right now, we expect it to be calculated
        # based on stuff in the structure, so we don't save it right now.
        if unpacker.known_max is not None:
            self.bufsize = unpacker.known_max

    def __repr__(self):
        guts = []
        for k in dir(self):
            # skip all the python generated gunk
            if k.startswith("__"):
                continue

            attr = getattr(self, k)
            # skip synthetic(), pack(), etc.
            if callable(attr):
                continue

            guts.append(f"{k}={attr}")
        return f"{self.__class__.__qualname__}({', '.join(guts)})"


class Buffer(Protobj):
    def __init__(self, unpacker):
        Protobj.__init__(self, unpacker)
        self.buf = unpacker.buf


class Struct(Protobj):
    pass


class Union(Protobj):
    @classmethod
    def synthetic(cls, data=[], fmt=""):
        self = cls.__new__(cls)
        self.__init__(MemoryUnpacker(struct.pack(fmt, *data)))
        return self


class Cookie(object):
    reply_type = None

    def __init__(self, conn, sequence, is_checked):
        self.conn = conn
        self.sequence = sequence
        self.is_checked = is_checked

    def reply(self):
        data = self.conn.wait_for_reply(self.sequence)
        return self.reply_type(data)

    def check(self):
        # Request is not void and checked.
        assert self.is_checked and self.reply_type is None, (
            "Request is not void and checked"
        )
        self.conn.request_check(self.sequence)

    def discard_reply(self):
        return self.conn.discard_reply(self.sequence)


class VoidCookie(Cookie):
    def reply(self):
        raise XcffibException("No reply for this message type")


class Extension(object):
    def __init__(self, conn, key=None):
        self.conn = conn

        if key is None:
            self.c_key = ffi.NULL
        else:
            self.c_key = key.c_key

    def send_request(
        self, opcode, data, cookie=VoidCookie, reply=None, is_checked=False
    ):
        data = data.getvalue()

        assert len(data) > 3, "xcb_send_request data must be ast least 4 bytes"

        xcb_req = ffi.new("xcb_protocol_request_t *")
        xcb_req.count = 2
        xcb_req.ext = self.c_key
        xcb_req.opcode = opcode
        xcb_req.isvoid = issubclass(cookie, VoidCookie)

        # XXX: send_request here will use the memory *before* the passed in
        # xcb_parts pointer in some cases, so we need to allocate some for it
        # to use, although we don't use it ourselves.
        #
        # http://lists.freedesktop.org/archives/xcb/2014-February/009307.html
        xcb_parts = ffi.new("struct iovec[4]")

        # Here we need this iov_base to keep this memory alive until the end of
        # the function.
        xcb_parts[2].iov_base = iov_base = ffi.new("char[]", data)  # noqa
        xcb_parts[2].iov_len = len(data)
        xcb_parts[3].iov_base = ffi.NULL
        xcb_parts[3].iov_len = -len(data) & 3  # is this really necessary?

        flags = lib.XCB_REQUEST_CHECKED if is_checked else 0

        seq = self.conn.send_request(flags, xcb_parts + 2, xcb_req)

        return cookie(self.conn, seq, is_checked)

    def __getattr__(self, name):
        if name.endswith("Checked"):
            real = name[: -len("Checked")]
            is_checked = True
        elif name.endswith("Unchecked"):
            real = name[: -len("Unchecked")]
            is_checked = False
        else:
            raise AttributeError(name)

        real = getattr(self, real)

        return functools.partial(real, is_checked=is_checked)


class List(Protobj):
    def __init__(self, unpacker, typ, count=None):
        Protobj.__init__(self, unpacker)
        self.typ = typ

        self.list = []
        old = unpacker.offset

        if isinstance(typ, str):
            self.list = list(unpacker.unpack("%d%s" % (count, typ)))
        elif count is not None:
            for _ in range(count):
                item = typ(unpacker)
                self.list.append(item)
        else:
            assert unpacker.known_max is not None
            while unpacker.offset < unpacker.known_max:
                item = typ(unpacker)
                self.list.append(item)

        self.bufsize = unpacker.offset - old

        self.raw = bytes(unpacker.buf[old : old + self.bufsize])

        assert count is None or count == len(self.list)

    def __str__(self):
        return str(self.list)

    def __len__(self):
        return len(self.list)

    def __iter__(self):
        return iter(self.list)

    def __getitem__(self, key):
        return self.list[key]

    def __setitem__(self, key, value):
        self.list[key] = value

    def __delitem__(self, key):
        del self.list[key]

    def __repr__(self):
        # the spec uses both CHAR8 and VOID to indicate strings, we stringify
        # both here. this is of course a heuristic, because it uses VOID to
        # mean other things, but this just debug code, so it's probably fine...
        if self.typ in ["c", "B"]:
            return "\"" + self.to_string() + "\""
        else:
            return "[" + ", ".join(repr(i) for i in self) + "]"

    def to_string(self):
        """A helper for converting a List of chars to a native string. Dies if
        the list contents are not something that could be reasonably converted
        to a string."""
        try:
            return "".join(chr(i[0]) for i in self)
        except TypeError:
            return "".join(chr(i) for i in self)

    def to_nullsep_string(self) -> list[str]:
        """A helper for converting a List of chars to a list of native
        strings, starting a new string each time a null (i.e. \\x00) is seen."""
        return self.to_string().split("\x00")

    def to_utf8(self):
        return b"".join(self).decode("utf-8")

    def to_atoms(self):
        """A helper for converting a List of chars to an array of atoms"""
        return struct.unpack("<" + "%dI" % (len(self) // 4), b"".join(self))

    def buf(self):
        return self.raw

    @classmethod
    def synthetic(cls, list=None):
        if list is None:
            list = []

        self = cls.__new__(cls)
        self.list = list[:]
        return self


class OffsetMap(object):
    def __init__(self, core):
        self.offsets = [(0, 0, core)]

    def add(self, offset, opcode, things):
        self.offsets.append((offset, opcode, things))
        self.offsets.sort(key=lambda x: x[0], reverse=True)

    def get_extension_item(self, extension, item):
        try:
            _, _, things = next(
                (k, opcode, v) for k, opcode, v in self.offsets if opcode == extension
            )
            return things[item]
        except StopIteration:
            raise IndexError(item)

    def __getitem__(self, item):
        try:
            offset, _, things = next(
                (k, opcode, v) for k, opcode, v in self.offsets if item >= k
            )
            return things[item - offset]
        except StopIteration:
            raise IndexError(item)


class Connection(object):
    """`auth` here should be '<name>:<data>', a format bequeathed to us from
    xpyb."""

    def __init__(self, display=None, fd=-1, auth=None):
        if auth is not None:
            [name, data] = auth.split(b":")

            c_auth = ffi.new("xcb_auth_info_t *")
            c_auth.name = ffi.new("char[]", name)
            c_auth.namelen = len(name)
            c_auth.data = ffi.new("char[]", data)
            c_auth.datalen = len(data)
        else:
            c_auth = ffi.NULL

        if display is None:
            display = ffi.NULL
        else:
            display = display.encode("latin1")

        i = ffi.new("int *")

        if fd > 0:
            self._conn = lib.xcb_connect_to_fd(fd, c_auth)
        elif c_auth != ffi.NULL:
            self._conn = lib.xcb_connect_to_display_with_auth_info(display, c_auth, i)
        else:
            self._conn = lib.xcb_connect(display, i)
        self.pref_screen = i[0]
        self.invalid()
        self._init_x()

    def _init_x(self):
        if core is None:
            raise XcffibException(
                "No core protocol object has been set.  Did you import xcffib.xproto?"
            )

        self.core = core(self)
        self.setup = self.get_setup()

        self._event_offsets = OffsetMap(core_events)
        self._error_offsets = OffsetMap(core_errors)
        self._setup_extensions()

    def _setup_extensions(self):
        for key, (_, events, errors) in extensions.items():
            reply = lib.xcb_get_extension_data(self._conn, key.c_key)
            # although it does not say so in the man page,
            # xcb_get_extension_data() will return NULL if the connection has
            # an error. let's guard against this, and do our own connection
            # checking in this case.
            if reply == ffi.NULL:
                self.invalid()
                raise XcffibException("uh oh... connection valid but xcb_get_extension_data() returned NULL?")
            self._event_offsets.add(reply.first_event, reply.major_opcode, events)
            self._error_offsets.add(reply.first_error, reply.major_opcode, errors)

    def __call__(self, key):
        return extensions[key][0](self, key)

    def invalid(self):
        if self._conn is None:
            raise XcffibException("Invalid connection.")
        err = lib.xcb_connection_has_error(self._conn)
        if err > 0:
            raise ConnectionException(err)

    def ensure_connected(f):
        """
        Check that the connection is valid both before and
        after the function is invoked.
        """

        @functools.wraps(f)
        def wrapper(*args):
            self = args[0]
            self.invalid()
            try:
                return f(*args)
            finally:
                self.invalid()

        return wrapper

    @ensure_connected
    def get_setup(self):
        self._setup = lib.xcb_get_setup(self._conn)

        # No idea where this 8 comes from either, similar complate to the
        # sizeof(xcb_generic_reply_t) below.
        buf = CffiUnpacker(self._setup, known_max=8 + self._setup.length * 4)

        return _setup(buf)

    @ensure_connected
    def get_screen_pointers(self):
        """
        Returns the xcb_screen_t for every screen
        useful for other bindings
        """
        root_iter = lib.xcb_setup_roots_iterator(self._setup)

        screens = [root_iter.data]
        for i in range(self._setup.roots_len - 1):
            lib.xcb_screen_next(ffi.addressof((root_iter)))
            screens.append(root_iter.data)
        return screens

    @ensure_connected
    def wait_for_event(self):
        e = lib.xcb_wait_for_event(self._conn)
        e = ffi.gc(e, c_free)
        self.invalid()
        return self.hoist_event(e)

    @ensure_connected
    def poll_for_event(self):
        e = lib.xcb_poll_for_event(self._conn)
        self.invalid()
        if e != ffi.NULL:
            return self.hoist_event(e)
        else:
            return None

    def has_error(self):
        return lib.xcb_connection_has_error(self._conn)

    @ensure_connected
    def get_file_descriptor(self):
        return lib.xcb_get_file_descriptor(self._conn)

    @ensure_connected
    def get_maximum_request_length(self):
        return lib.xcb_get_maximum_request_length(self._conn)

    @ensure_connected
    def prefetch_maximum_request_length(self):
        return lib.xcb_prefetch_maximum_request_length(self._conn)

    @ensure_connected
    def flush(self):
        return lib.xcb_flush(self._conn)

    @ensure_connected
    def generate_id(self):
        return lib.xcb_generate_id(self._conn)

    def disconnect(self):
        if self._conn is not None:
            lib.xcb_disconnect(self._conn)
            self._conn = None

    def _process_error(self, c_error):
        self.invalid()
        if c_error != ffi.NULL:
            error = self._error_offsets[c_error.error_code]
            buf = CffiUnpacker(c_error)
            raise error(buf)

    @ensure_connected
    def wait_for_reply(self, sequence):
        error_p = ffi.new("xcb_generic_error_t **")
        data = lib.xcb_wait_for_reply(self._conn, sequence, error_p)
        data = ffi.gc(data, c_free)

        try:
            self._process_error(error_p[0])
        finally:
            if error_p[0] != ffi.NULL:
                c_free(error_p[0])

        if data == ffi.NULL:
            # No data and no error => bad sequence number
            raise XcffibException("Bad sequence number %d" % sequence)

        reply = ffi.cast("xcb_generic_reply_t *", data)

        # this is 32 and not `sizeof(xcb_generic_reply_t) == 8` because,
        # according to the X11 protocol specs: "Every reply consists of 32 bytes
        # followed by zero or more additional bytes of data, as specified in the
        # length field."
        return CffiUnpacker(data, known_max=32 + reply.length * 4)

    @ensure_connected
    def request_check(self, sequence):
        cookie = ffi.new("xcb_void_cookie_t [1]")
        cookie[0].sequence = sequence

        err = lib.xcb_request_check(self._conn, cookie[0])
        self._process_error(err)

    def hoist_event(self, e):
        """Hoist an xcb_generic_event_t to the right xcffib structure."""
        if e.response_type == 0:
            return self._process_error(ffi.cast("xcb_generic_error_t *", e))

        # We mask off the high bit here because events sent with SendEvent have
        # this bit set. We don't actually care where the event came from, so we
        # just throw this away. Maybe we could expose this, if anyone actually
        # cares about it.
        response_type = e.response_type & 0x7F

        buf = CffiUnpacker(e)
        event = None

        # If the response is a GeGenericEvent then we can try to hoist this to relevant
        # extension event.
        if response_type == 35:
            # Extract the extension opcode and event
            extension, _, event_type, _ = buf.unpack("xB2xIH22xI", increment=False)
            try:
                # Try to find matching event. If this fails, an IndexError is raised and
                # we'll fall back to raising an GeGenericEvent
                event = self._event_offsets.get_extension_item(extension, event_type)
            except IndexError:
                pass

        if event is None:
            event = self._event_offsets[response_type]

        return event(buf)

    @ensure_connected
    def send_request(self, flags, xcb_parts, xcb_req):
        return lib.xcb_send_request(self._conn, flags, xcb_parts, xcb_req)

    @ensure_connected
    def discard_reply(self, sequence):
        return lib.xcb_discard_reply(self._conn, sequence)


# More backwards compatibility
connect = Connection


class Response(Protobj):
    def __init__(self, unpacker):
        Protobj.__init__(self, unpacker)

        # These (and the ones in Reply) aren't used internally and I suspect
        # they're not used by anyone else, but they're here for xpyb
        # compatibility.
        #
        # In some cases (e.g. creating synthetic events from memory), we don't
        # have the sequence number (since the event was fake), so only try to
        # get these attributes if we are really using a cffi buffer.
        if isinstance(unpacker, CffiUnpacker):
            resp = unpacker.cast("xcb_generic_event_t *")
            self.response_type = resp.response_type
            self.sequence = resp.sequence
        else:
            self.response_type = None
            self.sequence = None


class Reply(Response):
    def __init__(self, unpacker):
        Response.__init__(self, unpacker)

        # also for compat
        resp = unpacker.cast("xcb_generic_reply_t *")
        self.length = resp.length


class Event(Response):
    def __init__(self, unpacker):
        # This is here for debugging purposes!
        self.unpacker = unpacker

        Response.__init__(self, unpacker)

        # If this is a xcb_ge_generic_event_t (response type 35) then we need a few more fields
        if self.xge and isinstance(unpacker, CffiUnpacker):
            self.extension, self.length, self.event_type, self.full_sequence = (
                unpacker.unpack("xB2xIH22xI")
            )

            # There's some extra work to do if the event has data past the 32 byte boundary
            if self.length:
                # Calculate the size of the original buffer. This is 4 bytes short as it seems to omit the `full_sequence` field
                buffer_size = 32 + (self.length * 4) + 4

                # Create a buffer object that's the full size
                buffer = ffi.buffer(unpacker.cdata, buffer_size)

                # Copy the event to the new buffer and skip the `full_sequence` field
                buffer[32 : buffer_size - 5] = buffer[36 : buffer_size - 1]

                # Provide the resized buffer to the unpacker
                unpacker.buf = buffer

            # The xge events in xcbproto do not include the fields in the first ten bytes but
            # the generated code only skips four bytes so we need to adjust the starting offset
            unpacker.offset = struct.calcsize("BBHIH") - struct.calcsize("xB2x")


class Error(Response, XcffibException):
    def __init__(self, unpacker):
        Response.__init__(self, unpacker)
        XcffibException.__init__(self)
        self.code = unpacker.unpack("B", increment=False)


def pack_list(from_, pack_type):
    """Return the wire packed version of `from_`. `pack_type` should be some
    subclass of `xcffib.Struct`, or a string that can be passed to
    `struct.pack`. You must pass `size` if `pack_type` is a struct.pack string.
    """
    # We need from_ to not be empty
    if len(from_) == 0:
        return bytes()

    if pack_type == "c":
        if isinstance(from_, bytes):
            # Catch Python 3 bytes and Python 2 strings
            # PY3 is "helpful" in that when you do tuple(b'foo') you get
            # (102, 111, 111) instead of something more reasonable like
            # (b'f', b'o', b'o'), so we rebuild from_ as a tuple of bytes
            from_ = [bytes((b,)) for b in bytes(from_)]
        elif isinstance(from_, str):
            # Catch Python 3 strings and Python 2 unicode strings, both of
            # which we encode to bytes as utf-8
            # Here we create the tuple of bytes from the encoded string
            from_ = [bytes((b,)) for b in bytearray(from_, "utf-8")]
        elif isinstance(from_[0], int):
            # Pack from_ as char array, where from_ may be an array of ints
            # possibly greater than 256
            def to_bytes(v):
                for _ in range(4):
                    v, r = divmod(v, 256)
                    yield r

            from_ = [bytes((b,)) for i in from_ for b in to_bytes(i)]

    if isinstance(pack_type, str):
        return struct.pack("=%d%s" % (len(from_), pack_type), *from_)
    else:
        buf = io.BytesIO()
        for item in from_:
            # If we can't pack it, you'd better have packed it yourself. But
            # let's not confuse things which aren't our Probobjs for packable
            # things.
            if isinstance(item, Protobj) and hasattr(item, "pack"):
                buf.write(item.pack())
            else:
                buf.write(item)
        return buf.getvalue()


def wrap(ptr):
    c_conn = ffi.cast("xcb_connection_t *", ptr)
    conn = Connection.__new__(Connection)
    conn._conn = c_conn
    conn._init_x()
    conn.invalid()

    # ptr owns the memory for c_conn, even after the cast
    # we should keep it alive
    cffi_explicit_lifetimes[conn] = ptr

    return conn


def __DeviceTimeCoord_wrapper(typ, num_axes):
    def init(unpacker):
        i = typ(unpacker)
        i.num_axes = num_axes
        return i
