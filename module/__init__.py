from __future__ import division, absolute_import

import functools
import six
import struct

from .ffi import ffi, C, bytes_to_cdata, visualtype_to_c_struct

X_PROTOCOL = C.X_PROTOCOL
X_PROTOCOL_REVISION = C.X_PROTOCOL_REVISION

XCB_NONE = C.XCB_NONE
XCB_COPY_FROM_PARENT = C.XCB_COPY_FROM_PARENT
XCB_CURRENT_TIME = C.XCB_CURRENT_TIME
XCB_NO_SYMBOL = C.XCB_NO_SYMBOL

# For xpyb compatibility
NONE = XCB_NONE
CopyFromParent = XCB_COPY_FROM_PARENT
CurrentTime = XCB_CURRENT_TIME
NoSymbol = XCB_NO_SYMBOL

XCB_CONN_ERROR = C.XCB_CONN_ERROR
XCB_CONN_CLOSED_EXT_NOTSUPPORTED = C.XCB_CONN_CLOSED_EXT_NOTSUPPORTED
XCB_CONN_CLOSED_MEM_INSUFFICIENT = C.XCB_CONN_CLOSED_MEM_INSUFFICIENT
XCB_CONN_CLOSED_REQ_LEN_EXCEED = C.XCB_CONN_CLOSED_REQ_LEN_EXCEED
XCB_CONN_CLOSED_PARSE_ERR = C.XCB_CONN_CLOSED_PARSE_ERR
# XCB_CONN_CLOSED_INVALID_SCREEN = C.XCB_CONN_CLOSED_INVALID_SCREEN
# XCB_CONN_CLOSED_FDPASSING_FAILED = C.XCB_CONN_CLOSED_FDPASSING_FAILED


def type_pad(t, i):
    return -i & (3 if t > 4 else t - 1)


class Unpacker(object):

    def __init__(self, cdata, known_max=None, needs_pad=False):
        self.cdata = cdata
        self.size = 0
        self.offset = 0
        self.known_max = known_max
        if self.known_max is not None:
            self._resize(known_max)

    def _resize(self, increment):
        if self.offset + increment > self.size:
            if self.known_max is not None:
                assert self.size + increment <= self.known_max
            self.size = self.offset + increment
            self.buf = ffi.buffer(self.cdata, self.size)

    def pad(self, thing):
        if type(thing) in [Struct, Union]:
            if thing.is_fixed:
                size = thing.size
            else:
                size = 4
        else:
            size = struct.calcsize(thing)

        self.offset += type_pad(size, self.offset)

    def unpack(self, fmt, increment=True):
        size = struct.calcsize(fmt)
        self._resize(size)
        ret = struct.unpack_from("=" + fmt, self.buf, self.offset)

        if increment:
            self.offset += size
        return ret

    def cast(self, typ):
        assert self.offset == 0
        return ffi.cast(typ, self.cdata)


def popcount(n):
    return bin(n).count('1')


class XcffibException(Exception):
    """ Generic XcbException; replaces xcb.Exception. """
    pass


class ConnectionException(XcffibException):
    REASONS = {
        C.XCB_CONN_ERROR: (
            'xcb connection errors because of socket, '
            'pipe and other stream errors.'),
        C.XCB_CONN_CLOSED_EXT_NOTSUPPORTED: (
            'xcb connection shutdown because extension not supported'),
        C.XCB_CONN_CLOSED_MEM_INSUFFICIENT: (
            'malloc(), calloc() and realloc() error upon failure, '
            'for eg ENOMEM'),
        C.XCB_CONN_CLOSED_REQ_LEN_EXCEED: (
            'Connection closed, exceeding request length that server '
            'accepts.'),
        C.XCB_CONN_CLOSED_PARSE_ERR: (
            'Connection closed, error during parsing display string.'),
#        C.XCB_CONN_CLOSED_INVALID_SCREEN: (
#            'Connection closed because the server does not have a screen '
#            'matching the display.'),
#        C.XCB_CONN_CLOSED_FDPASSING_FAILED: (
#            'Connection closed because some FD passing operation failed'),
    }

    def __init__(self, err):
        XcffibException.__init__(
            self, self.REASONS.get(err, "Unknown connection error."))


class ProtocolException(XcffibException):
    pass


core = None
core_events = None
core_errors = None
setup = None

extensions = {}

# This seems a bit over engineered to me; it seems unlikely there will ever be
# a core besides xproto, so why not just hardcode that?
def _add_core(value, _setup, events, errors):
    if not issubclass(value, Extension):
        raise XcffibException("Extension type not derived from xcffib.Extension")
    if not issubclass(_setup, Struct):
        raise XcffibException("Setup type not derived from xcffib.Struct")

    global core
    global core_events
    global core_errors
    global setup

    core = value
    core_events = events
    core_errors = errors
    setup = _setup


def _add_ext(key, value, events, errors):
    if not issubclass(value, Extension):
        raise XcffibException("Extension type not derived from xcffib.Extension")
    extensions[key] = (value, events, errors)


class ExtensionKey(object):
    """ This definitely isn't needed, but we keep it around for compatibilty
    with xpyb.
    """
    def __init__(self, name):
        self.name = name

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, o):
        return self.name == o.name

    def __ne__(self, o):
        return self.name != o.name


class Protobj(object):

    """ Note: Unlike xcb.Protobj, this does NOT implement the sequence
    protocol. I found this behavior confusing: Protobj would implement the
    sequence protocol on self.buf, and then List would go and implement it on
    List. Additionally, as near as I can tell internally we only need the size
    of the buffer for cases when the size of things is unspecified. Thus,
    that's all we save.
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


class Struct(Protobj):
    pass


class Union(Protobj):
    pass


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
            "Request is not void and checked")
        self.conn.request_check(self.sequence)


class VoidCookie(Cookie):
    def reply(self):
        raise XcffibException("No reply for this message type")


class Extension(object):
    def __init__(self, conn, key=None):
        self.conn = conn
        if key is None:
            self.ext_name = None
        else:
            self.ext_name = key.name

    def send_request(self, opcode, data, cookie=VoidCookie, reply=None,
                     is_checked=False):
        data = data.getvalue()

        assert len(data) > 3, "xcb_send_request data must be ast least 4 bytes"

        xcb_req = ffi.new("xcb_protocol_request_t *")
        xcb_req.count = 2

        if self.ext_name is not None:
            key = ffi.new("struct xcb_extension_t *")
            key.name = bytes_to_cdata(self.ext_name.encode())
            # xpyb doesn't ever set global_id, which seems wrong, but whatever.
            key.global_id = 0
            xcb_req.ext = key
        else:
            xcb_req.ext = ffi.NULL

        xcb_req.opcode = opcode
        xcb_req.isvoid = issubclass(cookie, VoidCookie)

        xcb_parts = ffi.new("struct iovec[2]")
        xcb_parts[0].iov_base = bytes_to_cdata(data)
        xcb_parts[0].iov_len = len(data)
        xcb_parts[1].iov_base = ffi.NULL
        xcb_parts[1].iov_len = -len(data) & 3  # is this really necessary?

        flags = C.XCB_REQUEST_CHECKED if is_checked else 0

        seq = self.conn.send_request(flags, xcb_parts, xcb_req)

        return cookie(self.conn, seq, is_checked)

    def __getattr__(self, name):
        if name.endswith("Checked"):
            real = name[:-len("Checked")]
            is_checked = True
        elif name.endswith("Unchecked"):
            real = name[:-len("Unchecked")]
            is_checked = False
        else:
            raise AttributeError(name)

        real = getattr(self, real)

        return functools.partial(real, is_checked=is_checked)


class List(Protobj):
    def __init__(self, unpacker, typ, count=None):
        Protobj.__init__(self, unpacker)

        self.list = []
        old = unpacker.offset

        if isinstance(typ, str):
            self.list = list(unpacker.unpack(typ * count))
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

    def to_string(self):
        """ A helper for converting a List of chars to a native string. Dies if
        the list contents are not something that could be reasonably converted
        to a string. """
        return b''.join(self).decode('latin1')

    def to_atoms(self):
        """ A helper for converting a List of chars to an array of atoms """
        return struct.unpack("=" + "I" * (len(self) // 4), b''.join(self))

    def buf(self):
        return b''.join(self.list)

class Connection(object):

    def __init__(self, display=None, fd=-1, auth=None):
        if auth is not None:
            c_auth = ffi.new("xcb_auth_info_t *")
            if C.xpyb_parse_auth(auth, len(auth), c_auth) < 0:
                raise XcffibException("invalid xauth")
        else:
            c_auth = ffi.NULL
        display = display.encode('latin1')

        i = ffi.new("int *")
        i[0] = 0

        if fd > 0:
            self._conn = C.xcb_connect_to_fd(fd, c_auth)
        elif c_auth != ffi.NULL:
            self._conn = C.xcb_connect_to_display_with_auth(display, c_auth, i)
        else:
            self._conn = C.xcb_connect(display, i)
        self.pref_screen = i[0]
        self.invalid()

        self.core = core(self)
        self.setup = self.get_setup()

    def __call__(self, key):
        return extensions[key][0](self, key)

    def invalid(self):
        if self._conn is None:
            raise XcffibException("Invalid connection.")
        err = C.xcb_connection_has_error(self._conn)
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
        s = C.xcb_get_setup(self._conn)

        # No idea where this 8 comes from either, similar complate to the
        # sizeof(xcb_generic_reply_t) below.
        buf = Unpacker(s, known_max=8 + s.length * 4)

        return setup(buf)

    @ensure_connected
    def wait_for_event(self):
        e = C.xcb_wait_for_event(self._conn)
        e = ffi.gc(e, C.free)
        self.invalid()
        return self.hoist_event(e)

    @ensure_connected
    def poll_for_event(self):
        e = C.xcb_poll_for_event(self._conn)
        self.invalid()
        if e != ffi.NULL:
            return self.hoist_event(e)
        else:
            return None

    @ensure_connected
    def has_error(self):
        return C.xcb_connection_has_error(self._conn)

    @ensure_connected
    def get_file_descriptor(self):
        return C.xcb_get_file_descriptor(self._conn)

    @ensure_connected
    def get_maximum_request_length(self):
        return C.xcb_get_maximum_request_length(self._conn)

    @ensure_connected
    def prefetch_maximum_request_length(self):
        return C.xcb_prefetch_maximum_request_length(self._conn)

    @ensure_connected
    def flush(self):
        return C.xcb_flush(self._conn)

    @ensure_connected
    def generate_id(self):
        return C.xcb_generate_id(self._conn)

    def disconnect(self):
        self.invalid()
        return C.xcb_disconnect(self._conn)

    def _process_error(self, c_error):
        self.invalid()
        if c_error != ffi.NULL:
            error = core_errors[c_error.error_code]
            buf = Unpacker(c_error)
            raise error(buf)

    @ensure_connected
    def wait_for_reply(self, sequence):
        error_p = ffi.new("xcb_generic_error_t **")
        data = C.xcb_wait_for_reply(self._conn, sequence, error_p)
        data = ffi.gc(data, C.free)

        try:
            self._process_error(error_p[0])
        finally:
            if error_p[0] != ffi.NULL:
                C.free(error_p[0])

        if data == ffi.NULL:
            # No data and no error => bad sequence number
            raise XcffibException("Bad sequence number %d" % sequence)

        reply = ffi.cast("xcb_generic_reply_t *", data)

        # why is this 32 and not sizeof(xcb_generic_reply_t) == 8?
        return Unpacker(data, known_max=32 + reply.length * 4)

    @ensure_connected
    def request_check(self, sequence):
        cookie = ffi.new("xcb_void_cookie_t [1]")
        cookie[0].sequence = sequence

        err = C.xcb_request_check(self._conn, cookie[0])
        self._process_error(err)

    def hoist_event(self, e):
        """ Hoist an xcb_generic_event_t to the right xcffib structure. """
        if e.response_type == 0:
            return self._process_error(ffi.cast("xcb_generic_error_t *", e))

        if e.response_type > 128:
            # avoid circular imports
            from .xproto import ClientMessageEvent
            event = ClientMessageEvent
        else:
            assert core_events, "You probably need to import xcffib.xproto"
            event = core_events[e.response_type & 0x7f]

        buf = Unpacker(e)
        return event(buf)

    @ensure_connected
    def send_request(self, flags, xcb_parts, xcb_req):
        return C.xcb_send_request(self._conn, flags, xcb_parts, xcb_req)

# More backwards compatibility
connect = Connection


class Response(Protobj):
    def __init__(self, unpacker):
        Protobj.__init__(self, unpacker)

        # These (and the ones in Reply) aren't used internally and I suspect
        # they're not used by anyone else, but they're here for xpyb
        # compatibility.
        resp = unpacker.cast("xcb_generic_event_t *")
        self.response_type = resp.response_type
        self.sequence = resp.sequence


class Reply(Response):
    def __init__(self, unpacker):
        Response.__init__(self, unpacker)

        # also for compat
        resp = unpacker.cast("xcb_generic_reply_t *")
        self.length = resp.length


class Event(Response):
    pass


class Error(Response, XcffibException):
    def __init__(self, unpacker):
        Response.__init__(self, unpacker)
        XcffibException.__init__(self)
        self.code = unpacker.unpack('B', increment=False)


def pack_list(from_, pack_type):
    """ Return the wire packed version of `from_`. `pack_type` should be some
    subclass of `xcffib.Struct`, or a string that can be passed to
    `struct.pack`. You must pass `size` if `pack_type` is a struct.pack string.
    """

    if six.PY3:
        # If a string is passed as `from_` in Python 3, it has to be encoded
        if isinstance(from_, str):
            from_ = from_.encode('latin1')
        # PY3 is "helpful" in that when you do tuple(b'foo') you get
        # (102, 111, 111) instead of something more reasonable like
        # (b'f', b'o', b'o'), so we have to add this other special case.
        if isinstance(from_, bytes):
            from_ = [bytes([b]) for b in from_]

    if isinstance(pack_type, six.string_types):
        return struct.pack("=" + pack_type * len(from_), *tuple(from_))
    else:
        buf = six.BytesIO()
        for item in from_:
            # If we can't pack it, you'd better have packed it yourself...
            if isinstance(item, Struct):
                buf.write(item.pack())
            else:
                buf.write(item)
        return buf.getvalue()
