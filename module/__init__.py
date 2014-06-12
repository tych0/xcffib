from __future__ import division

import functools
import six
import struct

from .ffi import ffi, C, bytes_to_cdata

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

    def __init__(self, parent, offset, size=None):
        """
        Params:
        - parent: a bytes()
        - offset: the start of this offest in the bytes()
        - size: the size of this object (if none, then it is assumed to be
          len(parent))
        """

        if size is not None:
            assert size == 0 or len(parent) >= offset
        else:
            size = len(parent)
        self.bufsize = size


class Struct(Protobj):
    pass

class Union(Protobj):
    pass

class Cookie(object):
    reply_type = None
    def __init__(self, conn, sequence):
        self.conn = conn
        self.sequence = sequence

    def reply(self):
        data = self.conn.wait_for_reply(self.sequence)
        return self.reply_type(data, 0, len(data))

class VoidCookie(Cookie):
    def reply(self):
        raise XcffibException("No reply for this message type")

class Extension(object):
    def __init__(self, conn):
        self.conn = conn
        name = self.__class__.__name__[:-len('Extension')]
        if name == 'xproto':
            self.ext_name = None
        else:
            self.ext_name = name

    def send_request(self, opcode, data, cookie=VoidCookie, reply=None):
        data = data.getvalue()

        assert len(data) > 3, "xcb_send_request data must be ast least 4 bytes"

        self.conn.invalid()

        xcb_req = ffi.new("xcb_protocol_request_t *")
        xcb_req.count = 2

        if self.ext_name is not None:
            key = ffi.new("xcb_extension_t *")
            key.name = self.ext_name
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

        # TODO: support checked requests
        flags = 0
        seq = C.xcb_send_request(self.conn._conn, flags, xcb_parts, xcb_req)

        return cookie(self.conn, seq)

class List(Protobj):
    def __init__(self, parent, offset, count, typ, size=-1):
        Protobj.__init__(self, parent, offset, count * size)

        self.list = []

        if isinstance(typ, str):
            assert size > 0
            self.list = list(struct.unpack_from(typ * count, parent, offset))
            self.bufsize = count * size
        else:
            cur = offset
            for _ in range(count):
                item = typ(parent, cur, size)
                cur += item.bufsize
                self.list.append(item)
            self.bufsize = cur - offset

        assert count == len(self.list)

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

        if fd > 0:
            self._conn = C.xcb_connect_to_fd(fd, c_auth)
        elif c_auth != ffi.NULL:
            self._conn = C.xcb_connect_to_display_with_auth(display, c_auth, i)
        else:
            self._conn = C.xcb_connect(display, i)
        self.pref_screen = i[0]

        self.core = core(self)
        self.setup = self.get_setup()

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
        def wrapper(*args, **kwargs):
            self = args[0]
            self.invalid()
            try:
                return f(*args, **kwargs)
            finally:
                self.invalid()
        return wrapper

    @ensure_connected
    def get_setup(self):
        s = C.xcb_get_setup(self._conn)
        # No idea where this 8 comes from either :-)
        buf = ffi.buffer(s, 8 + s.length * 4)

        global setup
        return setup(buf, 0, len(buf))

    @ensure_connected
    def wait_for_event(self):
        # TODO: implement
        pass

    @ensure_connected
    def poll_for_event(self):
        # TODO: implement
        pass

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

    def _process_error(self, error_p):
        self.invalid()
        if error_p[0] != ffi.NULL:
            opcode = error_p[0].error_code
            # TODO: resolve the actual error class; also, should we free
            # error_p? It is actually allocated by xcb_wait_for_reply.
            raise XcffibException(opcode)

    @ensure_connected
    def wait_for_reply(self, sequence):
        error_p = ffi.new("xcb_generic_error_t **")
        data = C.xcb_wait_for_reply(self._conn, sequence, error_p)

        self._process_error(error_p)
        if data == ffi.NULL:
            # No data and no error => bad sequence number
            raise XcffibException("Bad sequence number %d" % sequence)

        reply = ffi.cast("xcb_generic_reply_t *", data)
        # why is this 32 and not sizeof(xcb_generic_reply_t) == 8?
        return bytes(ffi.buffer(data, 32 + reply.length * 4))


class Response(Protobj):
    def __init__(self, parent, offset, size):
        Protobj.__init__(self, parent, offset, size)

        # These (and the ones in Reply) aren't used internally and I suspect
        # they're not used by anyone else, but they're here for xpyb
        # compatibility.
        resp = ffi.cast("xcb_generic_event_t *", bytes_to_cdata(parent[offset:]))
        self.response_type = resp.response_type
        self.sequence = resp.sequence


class Reply(Response):
    def __init__(self, parent, offset, size):
        Response.__init__(self, parent, offset, size)

        resp = ffi.cast("xcb_generic_reply_t *", bytes_to_cdata(parent[offset:]))
        self.length = resp.length


class Event(Response):
    pass


class Error(Response, XcffibException):
    def __init__(self, parent, offset):
        Response.__init__(self, parent, offset)
        XcffibException.__init__(self)
        self.code = struct.unpack_from('B', parent)


def pack_list(from_, pack_type, count=None):
    """ Return the wire packed version of `from_`. `pack_type` should be some
    subclass of `xcffib.Struct`, or a string that can be passed to
    `struct.pack`. You must pass `size` if `pack_type` is a struct.pack string.
    """

    if isinstance(pack_type, six.string_types):
        return struct.pack(pack_type * len(from_), *tuple(from_))
    else:
        # from_ = List(from_, 0, -1, pack_type)
        # TODO: implement packing of lists that aren't base types.
        raise NotImplementedError("implement this correctly, fool")
